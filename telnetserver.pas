(* Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+ *)
(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit TelnetServer;

(* Implement a simple execution framework for a Telnet server. This uses a      *)
(* background thread to receive characters from a TCP/IP socket or stdin (a     *)
(* poll facility is available if threads are not being used), which are         *)
(* enqueued for use by ReadLn(INPUT) as well as more specialised input          *)
(* functions (i.e. with timeouts etc.) with a thread-safe lock.                 *)
(*                                                                              *)
(* The main program thread may use WriteLn(OUTPUT) etc. in the usual way,       *)
(* although online state should be monitored carefully.                         *)
(*                                                                              *)
(* Telnet options may be monitored and handled by callbacks, or by insertion of *)
(* an IAC character in the normal data flow. A change of online state may be    *)
(* detected similarly as a special case (nominally, this is option -1).         *)
(*                                                                              *)
(* NOTE THAT OPERATION MIGHT BE UNRELIABLE FOR FPC VERSIONS OLDER THAN 3.0.0.   *)
(* Symptoms seen include input working correctly but output not being           *)
(* redirected to the Telnet connection, or neither direction working properly.  *)
(*                                                                              *)
(*                                                              MarkMLl.        *)

{$if FPC_FULLVERSION < 030000 }
{$warning Possible reliability problems with FPC older than 3.0.0. }
{$endif FPC_FULLVERSION       }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Sockets, SyncObjs, TelnetCommon, TelnetBuffer;

const
  INVALID_SOCKET= TelnetCommon.INVALID_SOCKET;
  Iac= TelnetCommon.Iac;
  IacDo= TelnetCommon.IacDo;
  IacDont= TelnetCommon.IacDont;
  IacWill= TelnetCommon.IacWill;
  IacWont= TelnetCommon.IacWont;
  IacSb=  TelnetCommon.IacSb;
  IacSe= TelnetCommon.IacSe;

type
  PText= ^Text;
  PTextRec= ^TextRec;

{$macro on  }
{$define NOPUT:= PText(Nil)^ }

type
  SocketStates= (Unbound, Bound, Accepted, Authenticated);

  TTelnetServer = class;
  TelnetProc= procedure(telnet: TTelnetServer; const option: AnsiString);
  AvailableProc= procedure(telnet: TTelnetServer; available: integer);

  TTelnetServer= class(TTelnetCommon)
  strict private
    socketState: SocketStates;
    fSocket: TSocket;
    fOwnAddr: AnsiString;
    fClientAddr: AnsiString;
    fClientPort: integer;
    fPin: integer;
    fSessionPin: integer;
    ptrInput: PTextRec;
    ptrOutput: PTextRec;
    critSectOutput: TCriticalSection;
{$ifdef LCL }
    reentryOutput: longint;
{$endif LCL }
    fAnnounceCallback: TelnetProc;

    (* Prepare a server-style socket optimised for prompt reconnection.
    *)
    function prepareSocket(): boolean;

    (* This is a blocking call to read one 8-bit character at a time, silently
      reverting to TCP Listen state etc. as required. Note that this does not filter
      out e.g. Telnet IAC sequences.

       As a special case to support polled operation, the read may be deferred.
    *)
    function getNextByte(out b: byte; blocking: TBlocking= [IsBlocking]): boolean; override;

  protected

    procedure SetOnline(goOnline: boolean); override;

    (* Accumulate and enqueue client messages, handling Telnet IACs etc. minimally.
    *)
    procedure Execute; override;

    constructor Create(CreateSuspended: Boolean; var input, output: text;
                                                        port, pin: integer);
  public
    fHandlesClosed: boolean;
    fSavedInputTextRec: TextRec;        (* Have to be public to be accessible   *)
    fSavedOutputTextRec: TextRec;       (* to helper.                           *)

    (* Normally true, progress messages such as the session PIN will be written to
      StdErr.
    *)
    ProgressOnStdErr: boolean;

    (* If the port is < 0 input and output remain on StdIn/StdOut. If >=65536 the port
      number is randomised, if the PIN is <0 it will be randomised while is it is zero
      it will be suppressed.
    *)
    constructor Create(var input, output: text; port: integer= -1; pin: integer= 0);

    (* If the port is < 0 input and output remain on StdIn/StdOut. If >=65536 the port
      number is randomised, if the PIN is <0 it will be randomised while is it is zero
      it will be suppressed.
    *)
    constructor Create(port: integer= -1; pin: integer= 0);

    destructor Destroy; override;

    (* Register a Telnet option in the range -1 through 256 where -1 represents a
      connection/hangup and 256 is a final catch-all.

      The handler should expect either a standard three-byte option negotiation or
      a suboption ending with 0xf0, except that as a special case a hangup will be
      indicated by 0xff followed by 0x01 or 0x00 respectively (i.e. two bytes only).
    *)
    procedure RegisterOption(telnetOption: integer; proc: TelnetProc);

    (* Register an NVT control callback in the range 241..249.
    *)
    procedure RegisterControl(control: integer; proc: TelnetProc);

    (* This is called by the RTL code via a TextRec, and reads anything which has
      been enqueued by the background thread or an equivalent activity into the
      local buffer.
    *)
    procedure BufferFromPort;

    (* This is called by the RTL code via a TextRec whenever a Write() etc. fills
      the local buffer (see fpc_WriteBuffer()); since application-level data is
      involved any embedded IAC characters will be escaped. It will not be called
      on the completion of short Write() operations, instead FlushToPort below will
      be used.
    *)
    procedure BufferToPort;

    (* This is called by the RTL code via a TextRec on completion of a Write() etc.
      operation (see fpc_Write_End() and fpc_Writeln_End()) or if the application
      code calls Flush(OUTPUT); since application-level data is involved any
      embedded IAC characters will be escaped.
    *)
    procedure FlushToPort;

    (* Start up the background thread etc., generally after all registration has
      been done. Do not use this if input is from stdin and a prompt is required.
    *)
    function Run(bufferLimit: TBufferLimit= Normal): boolean; override;

    (* Using the main thread, read input and when available transfer to the buffer
      that supports ReadLn() etc. This is a blocking call and returns true when a
      character has been buffered, it is an alternative to running a background
      thread.

       Expect polled operation- whether blocking or non-blocking- to cause
      problems for all sorts of things, in particular the RTL's ReadLn() and
      all of the TTelnetCommon methods which ultimately rely on ReadCharTimeout()
      due to having no obvious way to exit its timeout loop.
    *)
    function Poll(blocking: boolean= true): boolean;

    (* Return a response to a client, with optional CRLF termination, and with no
      escaping of embedded IAC characters. Return false if the response cannot be
      sent, e.g. because this is being called in a hangup handler after the socket
      has been shut down.
    *)
    function Respond(const msg: ansistring; eol: boolean= false): boolean;

    (* Forcibly terminate the session, going back to a listening state. Assume that
      the bulk of the clearing up will be done by getNextByte().
    *)
    procedure Hangup(pause: integer=250); override;

    (* The enqueued received data exceeeds the byte threshold. This is called in the
      context of the background thread, and resets AvailableEventPrimed false.
    *)
    procedure SetOnAvailable(onAvailable: AvailableProc);

    (* This identifies the server's IP addresses as a space-separated string.
    *)
    property OwnAddr: AnsiString read fOwnAddr;

    (* This identifies the client at the other end of the current link.
    *)
    property ClientAddr: AnsiString read fClientAddr;

    (* This identifies the client at the other end of the current link.
    *)
    property ClientPort: integer read fClientPort;

    (* This is the session PIN if it is to be presented to the user other than via StdErr.
    *)
    property SessionPin: integer read fSessionPin;

    (* This is called in the context of the background thread immediately after the PIN
      has been allocated, including the case when it is zero.
    *)
    property AnnounceCallback: TelnetProc read fAnnounceCallback write fAnnounceCallback;

    (* The enqueued received data exceeeds the byte threshold. This is called in the
      context of the background thread, and resets AvailableEventPrimed false.
    *)
    property OnAvailable: AvailableProc write SetOnAvailable;

  end;


implementation

uses
  TelnetTextRec, TelnetPrivs, BaseUnix, ipaddressutils { , Errors }
                                        {$ifdef LCL } , Forms {$endif LCL } ;

type
  ETelnetForcedTermination= class(Exception);

  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890


procedure uninitialisedFunction(var t: TextRec);

begin
  raise EAccessViolation.Create('I/O handler for ' + t.name + ' uninitialised')
end { uninitialisedFunction } ;


constructor TTelnetServer.Create(CreateSuspended: Boolean; var input, output: text;
                                                        port, pin: integer);

var
  i: integer;
                                        (* If we close stdin/stdout handles     *)
const                                   (* it will prevent us from using fpRead *)
  closeHandles= false;                  (* etc. to get to the console should we *)
                                        (* ever need to do that. Also the       *)
begin                                   (* TextRecs won't be saved/restored.    *)
  inherited Create(CreateSuspended);

(* The input and output parameters will normally be the program's predefined    *)
(* INPUT and OUTPUT text devices, but need not be. In any event we need to      *)
(* check the TextRec object for the expected structure and behaviour as far as  *)
(* we are able, since in theory at least it is intended for internal use by the *)
(* RTL only.                                                                    *)

  if not TextRecValid(input) then       (* Will accept NOPUT as valid parameter *)
    fail;
  if not TextRecValid(output) then
    fail;
  if ThreadManagerInstalled() then
    inherited Create(CreateSuspended);

(* It is rare for TThread.Create() to fail, but if it should then we must not   *)
(* attempt to redirect INPUT and in particular OUTPUT in case it is being used  *)
(* for error messages. Note that ERROUTPUT should never be redirected, and that *)
(* it is this that should be used for error messages that must appear locally.  *)

  if not Assigned(self) then
    fail;
  FillByte(fSavedInputTextRec, SizeOf(TextRec), 0);
  with fSavedInputTextRec do begin
    name := '### INPUT ###';
    openfunc := @uninitialisedFunction;
    inoutfunc := @uninitialisedFunction;
    flushfunc := @uninitialisedFunction;
    closefunc := @uninitialisedFunction
  end;
  FillByte(fSavedOutputTextRec, SizeOf(TextRec), 0);
  with fSavedOutputTextRec do begin
    name := '### OUTPUT ###';
    openfunc := @uninitialisedFunction;
    inoutfunc := @uninitialisedFunction;
    flushfunc := @uninitialisedFunction;
    closefunc := @uninitialisedFunction
  end;
  ptrInput := @input;
  ptrOutput := @output;
  fHandlesClosed := closeHandles;
  fPortNumber := port;
  if port >= 65536 then                 (* Randomise because of ephemeral port  *)
    Randomize;
  BindTextRecs(self, input, output, closeHandles, fPortNumber);
  if (port >= 0) and (ptrOutput <> nil) then
    SetTextLineEnding(output, #$0d#$0a);
  ProgressOnStdErr := true;
  fSocket := INVALID_SOCKET;
  fClient := INVALID_SOCKET;
  fOwnAddr := '';
  fClientAddr := '';
  fClientPort := -1;
  fPin := pin;
  fSessionPin := 0;
  socketState := Unbound;
  critSectOutput := TCriticalSection.Create;
{$ifdef LCL }
  reentryOutput := 0;
{$endif LCL }
  fAnnounceCallback := nil
end { TTelnetServer.Create } ;


(* If the port is < 0 input and output remain on StdIn/StdOut. If >=65536 the port
  number is randomised, if the PIN is <0 it will be randomised while is it is zero
  it will be suppressed.
*)
constructor TTelnetServer.Create(var input, output: text;
                                                port: integer= -1; pin: integer= 0);

begin
  Create(true, input, output, port, pin)
end { TTelnetServer.Create } ;


(* If the port is < 0 input and output remain on StdIn/StdOut. If >=65536 the port
  number is randomised, if the PIN is <0 it will be randomised while is it is zero
  it will be suppressed.
*)
constructor TTelnetServer.Create(port: integer= -1; pin: integer= 0);

begin
  Create(NOPUT, NOPUT, port, pin)
end { TTelnetServer.Create } ;


destructor TTelnetServer.Destroy;

begin
  SetOnline(false);
  if ThreadManagerInstalled() then begin
    Terminate;

(* I was initially using                                                        *)
(*                                                                              *)
(*    PThread_kill(Handle, SIGHUP);      Anything stronger than HUP here        *)
(*    WaitFor                            would affect the entire process.       *)
(*                                                                              *)
(* here, but the kill caused major problems in a program which was also hooking *)
(* HUP and the WaitFor was just... well, it didn't work once I moved away from  *)
(* using HUP for non-obvious reasons.                                           *)

    fpShutdown(fClient, 2);
    CloseSocket(fClient);
    fpShutdown(fSocket, 2);
    CloseSocket(fSocket);
    repeat
      Sleep(1)
    until Finished
  end;
  critSectOutput.Free;
  if fClient <> INVALID_SOCKET then begin
    fpShutdown(fClient, 2);
    CloseSocket(fClient)
  end;
  if fSocket <> INVALID_SOCKET then begin
    fpShutdown(fSocket, 2);
    CloseSocket(fSocket)
  end;
  UnbindTextRecs(self, input, output);
  if ThreadManagerInstalled() then
    inherited Destroy
end { TTelnetServer.Destroy } ;


procedure TTelnetServer.SetOnline(goOnline: boolean);

begin
  fOnline := goOnline
end { TTelnetServer.SetOnline } ;


(* Register a Telnet option in the range -1 through 256 where -1 represents a
  connection/hangup and 256 is a final catch-all.

  The handler should expect either a standard three-byte option negotiation or
  a suboption ending with 0xf0, except that as a special case a hangup will be
  indicated by 0xff followed by 0x01 or 0x00 respectively (i.e. two bytes only).
*)
procedure TTelnetServer.RegisterOption(telnetOption: integer; proc: TelnetProc);

begin
  inherited RegisterOption(telnetOption, TelnetCommon.TelnetProc(proc))
end { TTelnetServer.RegisterOption } ;


(* Register an NVT control callback in the range 241..249.
*)
procedure TTelnetServer.RegisterControl(control: integer; proc: TelnetProc);

begin
  inherited RegisterControl(control, TelnetCommon.TelnetProc(proc))
end { TTelnetServer.RegisterControl } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890


(* This is called by the RTL code via a TextRec, and reads anything which has
  been enqueued by the background thread or an equivalent activity into the
  local buffer.
*)
procedure TTelnetServer.BufferFromPort;


  function minNZ(i1, i2: integer): integer; inline;

  begin
    if i1 < i2 then
      result := i1
    else
      result := i2;
    if result = 0 then
      result := 1
  end { minNZ } ;


begin                           (* Influenced by FileReadFunc() in text.inc.    *)
  Assert(fRunning, 'ReadLn() etc. are incompatible with polled operation.');
  FlushToPort;                          (* Nod to Nagle                         *)

(* minNZ() always returns at least 1, so even if the input buffer is empty we   *)
(* will block here until something's available.                                 *)

  ptrInput^.BufEnd := inputBuffer.Read(ptrInput^.Bufptr^,
                                minNZ(inputBuffer.BytesAvailable, ptrInput^.BufSize));
  ptrInput^.BufPos := 0
end { TTelnetServer.BufferFromPort } ;


(* This is called by the RTL code via a TextRec whenever a Write() etc. fills
  the local buffer (see fpc_WriteBuffer()); since application-level data is
  involved any embedded IAC characters will be escaped. It will not be called
  on the completion of short Write() operations, instead FlushToPort below will
  be used.
*)
procedure TTelnetServer.BufferToPort;

type
  charArray= array[0..255] of AnsiChar;

var
  i: integer;

begin                           (* Influenced by FileWriteFunc() in text.inc.   *)
  critSectOutput.Enter;
{$ifdef LCL }
  reentryOutput += 1;           (* We're already thread-safe                    *)
  while (reentryOutput <> 1) and (GetCurrentThreadId() = MainThreadId) do
    Application.ProcessMessages;
{$endif LCL }

(* I'd be far happier if reentry were controlled at a higher level, i.e. around *)
(* fpc_WriteBuffer() etc. since Write(OUTPUT) etc. may in theory at least be    *)
(* called simultaneously by multiple threads and GUI event handlers. In         *)
(* practice I think the situation is marginally safe if output is going to the  *)
(* console or a file on disc etc., but is much less safe when something like a  *)
(* TCP socket is being used.                                                    *)

  try
    if ptrOutput^.BufPos = 0 then
      exit;
    if fSocket < 0 then
      i := fpWrite(1, ptrOutput^.Bufptr^, ptrOutput^.BufPos)
    else
      i := SendBuffer(ptrOutput^.Bufptr^, ptrOutput^.BufPos);
    if i < 0 then
      inputBuffer.Hungup;               (* Might result in an ETelnetHangup     *)
    if i <> ptrOutput^.BufPos then
      InOutRes := 101;
    ptrOutput^.BufPos := 0
  finally
{$ifdef LCL }
    reentryOutput -= 1;         (* We're already thread-safe                    *)
{$endif LCL }
    critSectOutput.Leave
  end
end { TTelnetServer.BufferToPort } ;


(* This is called by the RTL code via a TextRec on completion of a Write() etc.
  operation (see fpc_Write_End() and fpc_Writeln_End()) or if the application
  code calls Flush(OUTPUT); since application-level data is involved any
  embedded IAC characters will be escaped.
*)
procedure TTelnetServer.FlushToPort;

begin                           (* Influenced by FileFlushFunc() in text.inc.   *)
  BufferToPort
end { TTelnetServer.FlushToPort } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890


(* This is a blocking call to read one 8-bit character at a time, silently
  reverting to TCP Listen state etc. as required. Note that this does not filter
  out e.g. Telnet IAC sequences.

  As a special case to support polled operation, the read may be deferred.
*)
function TTelnetServer.getNextByte(out b: byte; blocking: TBlocking= [IsBlocking]): boolean;

label
  listenAgain;

const
  deferredListen: boolean= false;       (* Static variable                      *)

var
  sockAddr: TInetSockAddr;
  sockLen: longint;
  readSet: TFDSet;
  timeout: TTimeVal;
  flags: cint;
  deferral: jmp_buf;


  (* Read a PIN terminated by any non-digit, return -1 if malformed.
  *)
  function readPin(): integer;

  const
    lastAttempt: TDateTime= 0.0;        (* Static variable                      *)
    willEcho: string[3]= Iac + IacWill + AnsiChar(1);
    wontEcho: string[3]= Iac + IacWont + AnsiChar(1);
    star: string[1]= '*';
    crlf: string[2]= #$0d#$0a;

  var
    c: AnsiChar;
    s: AnsiString= '';
    scratch: string[3];

  begin
    Assert(IsBlocking in blocking, 'PIN authentication and non-blocking poll not implemented.');
// TODO : Non-blocking poll incompatible with PIN authentication.
    fpSend(fClient, @willEcho[1], 3, 0);
    fpRecv(fClient, @scratch[1], 3, 0); (* Ignore response                      *)

// TODO : Any unsolicited option will be incorrectly interpreted as a PIN.
// The case of interest here is if the user does something like a ^] mode char
// while this code is actually waiting for a four-character PIN.

  (* inputBuffer is not being used yet, so there is no need to signal a hangup  *)
  (* if a send or recv fails here. A bit of rate-limiting just in case...       *)

    if Now() - lastAttempt < 7.5 * (1 / SecsPerDay) then
      Sleep(15000);
    lastAttempt := Now();
    try
      while true do begin
        if fpRecv(fClient, @c, 1, 0) < 1 then
          exit(-1);
        if not (c in ['0'..'9']) then
          break;

(* Trying to echo stars here is definitely more trouble than it's worth, since  *)
(* in order to do it properly one needs to not only tell the client that we'll  *)
(* handle echoing but also use a suboption sequence to change the line mode...  *)
(* which will have a response of indeterminate length. It's simple enough at    *)
(* the application level, when the state has progressed to Authenticated and    *)
(* the option/suboption parsing and callback is functional, but not here.       *)

//        fpSend(fClient, @star[1], 1, 0);
        s += c
      end;
      if not TryStrToInt(s, result) then
        result := -1;
      Sleep(175);
      while fpRecv(fClient, @c, 1, MSG_DONTWAIT) > 0 do
        Sleep(37);
      fpSend(fClient, @crlf[1], 2, 0);
    finally
      fpSend(fClient, @wontEcho[1], 3, 0);
      fpRecv(fClient, @scratch[1], 3, 0) (* Ignore response                     *)
    end
  end { readPin } ;


  function pinToStr(p: integer): AnsiString;

  begin
    result := IntToStr(p);
    while Length(result) < 4 do
      result := '0' + result
  end { pinToStr } ;


begin
  result := true;
  b := GetNextByteDefaultValue;         (* Printable character eases debugging  *)
  if deferredListen then
    LongJmp(deferral{%H-}, 1);

(* The one situation in which this function doesn't block is if it detects a    *)
(* Telnet hangup, in which case if an in-band notification has been requested   *)
(* it will insert an IAC in the data stream. In that case the reversion to a    *)
(* listening state will be deferred, but is enforced by the LongJmp() above.    *)

  if PortNumber < 0 then
    if IsDeferred in blocking then
      result := false
    else
      repeat
        if Terminated then
          Raise ETelnetForcedTermination.Create('Forced thread termination while receiving');
        if not (IsBlocking in blocking) then begin
          fpFD_ZERO(readSet);
          fpFD_SET(0, readSet);
          timeout.tv_sec := 0;
          timeout.tv_usec := 10000;
          flags := fpSelect(1, @readSet, nil, nil, @timeout);
          if flags < 0 then             (* Error, check for termination         *)
            continue
          else
            if flags = 0 then           (* No character, return no character    *)
              exit(false)
        end
      until fpRead(0, b, 1) = 1 (* Note: blocks but see "get out clause" above  *)
// TODO : Does NL need to be converted to CR for stdin on unix?
  else begin
listenAgain:
    case socketState of
      Unbound:  Assert(true, 'Unexpected Unbound state in getNextByte().');
      Bound:    while socketState < Accepted do begin

(* The socket on which we are listening has been bound, and the UI has had the  *)
(* opportunity to tell the user particularly if it is a random choice.          *)
(* Optionally announce to the application code what socket number is about to   *)
(* be listened on and what PIN will be expected, this might e.g. be written to  *)
(* a file which could be passed to a collaborating client as a form of two-     *)
(* factor authentication.                                                       *)

                  while fpListen(fSocket, 1) <> 0 do
                    if Terminated then
                      Raise ETelnetForcedTermination.Create('Forced thread termination while listening');
                  fSessionPin := fPin;
                  if fSessionPin < 0 then
                    fSessionPin := 1 + Random(9999);
                  if Assigned(fAnnounceCallback) then
                    fAnnounceCallback(self, IntToStr(fPortNumber) + ',' + pinToStr(fSessionPin));
                  if IsBlocking in blocking then begin
                    timeout.tv_sec := 0; (* Treat this as a blocking call       *)
                    timeout.tv_usec := 0
                  end else begin
                    timeout.tv_sec := 0; (* Treat this as a non-blocking call   *)
                    timeout.tv_usec := 10000 (* with this timeout in uSec.      *)
                  end;
                  if fpSetSockOpt(fSocket, SOL_SOCKET, SO_RCVTIMEO, @timeout, SizeOf(timeout)) < 0 then begin
            {$if declared(StrError) }
                    WriteLn(stderr, 'SetSockopt error ' + IntToStr(SocketError) + ': ' + StrError(SocketError))
            {$endif declared        }
                  end;
                  sockLen := SizeOf(sockAddr);
                  fClient := fpAccept(fSocket, @sockAddr, @sockLen); (* See timeout above *)
                  if (fClient < 0) and not (IsBlocking in blocking) then
                    exit(false);        (* No incoming connection, return no character *)
                  if Terminated then
                    Raise ETelnetForcedTermination.Create('Forced thread termination while accepting');
                  if fClient >= 0 then begin
                    fClientAddr := NetAddrToStr(sockAddr.sin_addr);
                    fClientPort := sockAddr.sin_port;
                    socketState := Accepted
                  end;
                  result := false
                end;
      Accepted: begin
                  inputBuffer.Hungup(false);
                  if connectionStateChanging(true) then begin
                    deferredListen := true;
                    if SetJmp(deferral) = 0 then begin
                      b := Ord(Iac);
                      exit(true)
                    end;
                    deferredListen := false
                  end;

(* The socket connection has been established, but don't set fOnline yet since  *)
(* this would tell the main thread that it could use ReadLn(INPUT) etc. which   *)
(* would be guaranteed to mess up any PIN check.                                *)

                  if fSessionPin > 0 then begin
                    if ProgressOnStdErr then
                      WriteLn(StdErr, '# Session PIN is ', pinToStr(fSessionPin));
                    Respond('Enter session PIN: ');
// TODO : Consider PIN state machine to allow non-blocking polled operation.
                    if readPin() <> fSessionPin then begin  (* Blocking call    *)
                      Respond(#$0d#$0a + 'PIN rejected.', true);
                      if ProgressOnStdErr then
                        WriteLn(StdErr, '# Session PIN from ', fClientAddr, ' rejected');
                      Hangup(0)
                    end else begin      (* Good PIN                             *)
                      if ProgressOnStdErr then
                        WriteLn(StdErr, '# Session PIN from ', fClientAddr, ' accepted');
                      socketState := Authenticated
                    end
                  end else              (* PIN check not requested              *)
                    socketState := Authenticated;

(* Finally...                                                                   *)

                  if socketState = Authenticated then begin
                    SetOnline(true);
                    result := false     (* ...but don't actually have data yet  *)
                  end
                end
    otherwise                           (* Authenticated state                  *)

(* The PIN check was successful if requested and fOnline has been set. Continue *)
(* with normal I/O, i.e. using either ReadLn(INPUT) or the additional calls     *)
(* with timeouts etc.                                                           *)

      if IsBlocking in blocking then begin
        flags := 0;
        sockLen := 1                    (* Terminate if < 1 character received  *)
      end else begin
        flags := MSG_DONTWAIT;
        sockLen := 0                    (* Terminate if result is -ve           *)
      end;
      flags := fpRecv(fClient, @b, 1, flags); (* Note: might or might not block *)
      if flags < sockLen then begin     (* Result visible for debugging         *)
        if (SocketError = ESysEAGAIN) and not (IsBlocking in blocking) then
          exit(false);
        if Terminated then
          Raise ETelnetForcedTermination.Create('Forced thread termination while receiving');
        SetOnline(false);               (* Keep peer address info in case we    *)
        CloseSocket(fClient);           (* want to only accept reconnection     *)
        fClient := INVALID_SOCKET;      (* from the same host.                  *)
        socketState := Bound;
        clearBuffer;
        inputBuffer.Hungup;             (* Might result in an ETelnetHangup     *)
        if connectionStateChanging(false) then begin
          deferredListen := true;
          if SetJmp(deferral) = 0 then begin
            b := Ord(Iac);
            exit(true)
          end;
          deferredListen := false
        end;
        goto listenAgain
      end
    end
  end
end { TTelnetServer.getNextByte } ;


(* Accumulate and enqueue client messages, handling Telnet IACs etc. minimally.
*)
procedure TTelnetServer.Execute;

begin
  if PortNumber < 0 then                (* Assume that stdin is immediately     *)
    SetOnline(true);                    (* online and never hangs up.           *)
  repeat
    try
      readAndEnqueue();
      except
        on E: ETelnetForcedTermination do
          break
      else
        raise
      end
    until Terminated
end { TTelnetServer.Execute } ;


(* Prepare a server-style socket optimised for prompt reconnection.
*)
function TTelnetServer.prepareSocket(): boolean;

var
  sockAddr: TInetSockAddr;
  sz: integer;
  randomPort: boolean;


  function check_suitable_port(port: longint): boolean; (* Name from RFC 6056 *)

  var
    i, j: integer;
    k: word;
    tcp: TStringList;
    line: AnsiString;

  begin
    result := true;                     (* On error, expect fpBind() to fail    *)
    tcp := TStringList.Create;
    try
      tcp.LoadFromFile('/proc/net/tcp');
      for i := 1 to tcp.Count - 1 do begin
        line := tcp[i];

(* Expect each line to look like this                                           *)
(*
  0: 010011AC:0035 00000000:0000 0A 00000000:00000000 00:00000000 00000000...   *)
(*                                                                              *)
(* We could simplify this by importing StrUtils, but I'm reluctant to add stuff *)
(* to the overall program if it's only used in one place.                       *)

        Delete(line, 1, Pos(':', line) + 1);
        Delete(line, 1, Pos(':', line));
        SetLength(line, Pos(' ', line) - 1);
        Val('$' + line, j, k);
        if (k = 0) and (j = port) then
          exit(false)
      end
    finally
      tcp.Free
    end
  end { check_suitable_port } ;


begin
  result := true;
  if PortNumber >= 0 then begin
    fSocket := fpSocket(PF_INET, SOCK_STREAM, 0);
    if fSocket < 0 then begin
      fSocket := INVALID_SOCKET;
      exit(false)
    end;

(* If the program terminates itself as the result of a received command, we     *)
(* need to make sure that the TCP stack cleans things up promptly. Treat a      *)
(* failure here as an irritant which might result in a problem if the program   *)
(* is restarted within (in the case of Linux 6.0) a minute, but not as an error *)
(* which merits possibly-confusing output.                                      *)

    sz := 1;
    if fpSetSockOpt(fSocket, SOL_SOCKET, SO_REUSEADDR, @sz, SizeOf(sz)) < 0 then begin
      sz := SocketError;
  //    WriteLn(StdErr, 'SetSockopt error ' + IntToStr(SocketError) + ': ' + StrError(SocketError))
    end;
    FillChar(sockAddr{%H-}, SizeOf(sockAddr), 0);
    sockAddr.sin_family:= AF_INET;
    sockAddr.sin_addr.s_addr:= 0;
    randomPort := fPortNumber > 65535;
    while true do begin                 (* This is really no better than a      *)
      if randomPort then                (* label for a GOTO.                    *)
        repeat                                  (* RFC 6056 specifies using the *)
          fPortNumber := 1024 + Random(64512)   (* largest range possible here. *)
        until check_suitable_port(PortNumber);
      sockAddr.sin_port:= hToNS(PortNumber);
      if fpBind(fSocket, @sockAddr, SizeOf(sockAddr)) < 0 then
        if not randomPort then
          exit(false)                   (* Unable to open required port, fail   *)
        else
          continue                      (* Unable to open random port, redo     *)
      else begin
        socketState := Bound;
        if fPin < 0 then
          Randomize;
        fOwnAddr := GetOwnIpAddresses(true);
        exit(true)                      (* Opened port successfully             *)
      end
    end
  end
end { TTelnetServer.prepareSocket } ;


(* Start up the background thread etc., generally after all registration has
  been done. Do not use this if input is from stdin and a prompt is required.
*)
function TTelnetServer.Run(bufferLimit: TBufferLimit= Normal): boolean;

// TODO : Buffer size.

begin
  result := true;
  ReportPrivilege('Initial');           (* Initial state of program             *)
  try
    if not prepareSocket() then
      exit(false)
  finally
    RelinquishCapabilities;
    RelinquishRootIdentity;
    ReportPrivilege('Residual')         (* Was relinquish successful?           *)
  end;
  inputBuffer.BufferLimit := bufferLimit;
  if ThreadManagerInstalled() then
{$if FPC_FULLVERSION >= 020600 }
    Start;
{$else                         }
    Resume;
{$endif FPC_FULLVERSION        }
  fRunning := true                      (* Not relying on Poll()                *)
end { TTelnetServer.Run } ;


(* Using the main thread, read input and when available transfer to the buffer
  that supports ReadLn() etc. This is a blocking call and returns true when a
  character has been buffered, it is an alternative to running a background
  thread.

   Expect polled operation- whether blocking or non-blocking- to cause
  problems for all sorts of things, in particular the RTL's ReadLn() and
  all of the TTelnetCommon methods which ultimately rely on ReadCharTimeout()
  due to having no obvious way to exit its timeout loop.
*)
function TTelnetServer.Poll(blocking: boolean= true): boolean;

var
  xBlocking: TBlocking= [IsBlocking];

(* I could probably get away with casting true to [IsBlocking] here, but feel   *)
(* that the risk is unjustified even if protected by an assertion.              *)

begin
  Assert(not fRunning, 'Poll() should not be used when a background thread is already running.');
  if not blocking then
    xBlocking := [];
  if PortNumber < 0 then begin
    if not Online then
      result := ReadAndEnqueue(xBlocking + [IsDeferred])
    else
      result := ReadAndEnqueue(xBlocking); (* False for initial poll only. Assume *)
    SetOnline(true)                     (* that once online stdin never hangs up. *)
  end else
    if fSocket = INVALID_SOCKET then begin
      ReportPrivilege('Initial');       (* Initial state of program             *)
      try
        result := prepareSocket()       (* Unbound -> Bound if successful       *)
      finally
        RelinquishCapabilities;
        RelinquishRootIdentity;
        ReportPrivilege('Residual')     (* Was relinquish successful?           *)
      end
    end else
      try
        if socketState < Authenticated then
          result := ReadAndEnqueue(xBlocking + [IsDeferred])
        else
          result := ReadAndEnqueue(xBlocking)
      except
        on E: ETelnetForcedTermination do
          exit(false)
      else
        raise
      end
end { TTelnetServer.Poll } ;


(* Return a response to a client, with optional CRLF termination, and with no
  escaping of embedded IAC characters. Return false if the response cannot be
  sent, e.g. because this is being called in a hangup handler after the socket
  has been shut down.
*)
function TTelnetServer.Respond(const msg: ansistring; eol: boolean= false): boolean;

begin
  if PortNumber < 0 then begin
    Write(msg);
    if eol then
      WriteLn;
    Flush(output);
    result := true
  end else
    if socketstate >= Accepted then
      result := inherited Respond(msg, eol)
    else
      result := false
end { TTelnetServer.Respond } ;


(* Forcibly terminate the session, going back to a listening state. Assume that
  the bulk of the clearing up will be done by getNextByte().
*)
procedure TTelnetServer.Hangup(pause: integer=250);

begin
  if pause > 0 then                     (* Allow time for any Telnet option     *)
    Sleep(pause);                       (* handshakes to complete etc.          *)
  SetOnline(false);
  if fClient <> INVALID_SOCKET then
    fpShutdown(fClient, 2);             (* Keep peer address info in case we    *)
  socketState := Bound                  (* want to reject reconnections from    *)
end { TTelnetServer.Hangup } ;          (* any other host.                      *)


(* The enqueued received data exceeeds the byte threshold. This is called in the
  context of the background thread, and resets AvailableEventPrimed false.
*)
procedure TTelnetServer.SetOnAvailable(onAvailable: AvailableProc);

begin
  inherited SetOnAvailable(TelnetCommon.AvailableProc(onAvailable))
end { TTelnetServer.SetOnAvailable } ;


end.

