(* Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+ *)
(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit TelnetCommon;

(* This provides an ancestor class for TTelnetServer and TTelnetClient, noting  *)
(* in particular that most of the option handling code is symmetrical. There is *)
(* however a significant difference: once running a server persists over client *)
(* disconnections (explicit or the result of a network error), while a client   *)
(* considers a disconnection as the termination if the current session even if  *)
(* it is prepared to attempt to establish a new one.            MarkMLl.        *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Sockets, TelnetBuffer;

const
  INVALID_SOCKET= -1;
  GetNextByteDefaultValue= $55;         (* Printable character eases debugging  *)

  Iac= #255;
  IacDont= #254;
  IacDo= #253;
  IacWont= #252;
  IacWill= #251;
  IacSb=  #250;
  IacSe= #240;

(* Below are rarely used, but are described in RFC 854 as NVT Control Functions. *)

  IacGA= #249;                          (* Go-ahead (normally suppressed)       *)
  IacEL= #248;                          (* Erase Line                           *)
  IacEC= #247;                          (* Erase character                      *)
  IacAYT= #246;                         (* Are You There                        *)
  IacAO= #245;                          (* Abort output                         *)
  IacIP= #244;                          (* Interrupt Process                    *)
  IacBRK= #243;                         (* Break                                *)
  IacDM= #242;                          (* Data mark                            *)
  IacNop= #241;                         (* No operation                         *)

(* The various functions with timeouts each have a default, in mSec; this is    *)
(* only really relevant to operation with a background thread. If operation is  *)
(* polled, any attempt to use a timeout larger than the defined timeout will    *)
(* be trapped by an assertion, if the timeout is less than the assertion it     *)
(* won't have a direct effect but instead execution will pause for a defined    *)
(* time before I/O status is checked.                                           *)

  DefaultTimeout= 10;                   (* For ReadLnTimeout() etc., mSec       *)
  TimeoutCutoff= DefaultTimeout;        (* Before error if Poll is being used   *)
  PauseInLieu= DefaultTimeout;          (* Start of ReadLnTimeout() if Poll()   *)
                                        (* is being used.                       *)
type
  LineStates= (Reading, SeenIac, SeenIacReq);
  ReadLnStatus= (ReadLnIncomplete, ReadLnComplete, ReadLnTimedout);
  BBlocking= (IsBlocking, IsDeferred);
  TBlocking= set of BBlocking;

  TTelnetCommon= class;
  TelnetProc= procedure(telnet: TTelnetCommon; const option: AnsiString);
  AvailableProc= procedure(telnet: TTelnetCommon; available: integer);
  TTelnetIdle= procedure(telnet: TTelnetCommon);

  TTelnetCommon= class(TThread)
  strict private
    iacReq: AnsiChar;
    lineState: LineStates;
    fOnAvailable: AvailableProc;
    registeredOptions: array[-1..256] of TelnetProc;
    noticed: array[-1..256] of boolean; (* Control addition of option messages  *)
    fOptions: TStringList;              (* to this stringlist acting as a FIFO  *)
    nvtExpansions: array[241..249] of AnsiString; (* Expansions of NVT Controls *)
    nvtControls: array[241..249] of TelnetProc;
    procedure onAvailableShim;
  protected
    fClient: TSocket;
    fPortNumber: integer;
    inputBuffer: TByteBuffer;
    fOnline: boolean;                   (* Managed entirely by derived class    *)
    fRunning: boolean;                  (* Is this relying on Poll()?           *)
    procedure SetOnline(goOnline: boolean); virtual; abstract;
    procedure Hangup(pause: integer=250); virtual; abstract;

    (* Send arbitrary data with escaping of IAC, returning the number of bytes
      actually sent or <0 on error (typically a hangup). Note that the returned
      value might be larger than the number of bytes specified as the parameter.
    *)
    function SendBuffer(const buffer; count: longint): longint;

    (* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
      actually read or <0 on error (typically a hangup).
    *)
    function RecvBuffer(const buffer; count: longint): longint;

    (* This is a blocking call to read one 8-bit character at a time, silently
      reverting to TCP Listen state etc. as required. Note that this does not filter
      out e.g. Telnet IAC sequences.

       As a special case to support polled operation, the read may be deferred.
    *)
    function getNextByte(out b: byte; blocking: TBlocking= [IsBlocking]): boolean; virtual; abstract;

    (* This is a blocking call to read and process a single byte, filtering out
      Telnet IAC sequences etc. Return FALSE on error.

       As a special case to support polled operation, the read may be deferred.
    *)
    function readAndEnqueue(blocking: TBlocking= [IsBlocking]): boolean;

    (* Clear the buffer between the background thread and foreground RTL etc.
    *)
    procedure clearBuffer;

    (* Return a noticed option and remove it from the FIFO.
    *)
    function getOption(): AnsiString;

    (* The application code's getNextByte() has noticed that the connection state is
      changing. This might have the effect of triggering the [-1] registered option
      etc. Return true if the application function should also return a dummy IAC
      etc.
    *)
    function connectionStateChanging(connecting: boolean): boolean;

    (* The enqueued received data exceeeds a certain number of bytes (if threshold
      is > 0) or lines (if threshold < 0). This is called in the context of the
      main thread, and resets AvailableEventPrimed false.
    *)
    procedure SetOnAvailable(onAvailable: AvailableProc);

{$if FPC_FULLVERSION < 030000 }
    (* This doesn't have quite the same semantics as the "official" Finished
      property implemented by FPC v3, but is near enough for test purposes.
    *)
    function GetFinished(): boolean;
{$endif FPC_FULLVERSION       }

  public

    (* Normally false, set this true if a "noticed" option is to insert an IAC marker
      in the data stream.
    *)
    IacInBand: boolean;

    (* Callback for the API receive functions. This is called in the context of the
      background thread.
    *)
    TelnetIdle: TTelnetIdle;

    (* The number of hangups seen.
    *)
    Hangups: integer;

    (* The threshold, in bytes (> 0) or lines (< 0) received, before the OnAvailable
      event fires.
    *)
    Threshold: integer;

    (* Initially false, this must be set true before OnAvailable is operative and
      is reset false to prevent repeated entry of the handler. OnAvailable will not
      fire again until this is set true.
    *)
    AvailableEventPrimed: boolean;

    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;

    (* Start up the background thread etc., generally after all registration has
      been done. Do not use this if input is from stdin and a prompt is required.
    *)
    function Run(bufferSize: TBufferLimit= Normal): boolean; virtual; abstract;

    (* Register a Telnet option in the range -1 through 256 where -1 represents a
      connection/hangup and 256 is a final catch-all.

      The handler should expect either a standard three-byte option negotiation or
      a suboption ending with 0xf0, except that as a special case a hangup will be
      indicated by 0xff followed by 0x01 or 0x00 respectively (i.e. two bytes only).
    *)
    procedure RegisterOption(telnetOption: integer; proc: TelnetProc);

    (* Register a Telnet option in the range -1 through 256 where -1 represents a
      connection/hangup and 256 is a final catch-all.

      Either a standard three-byte option negotiation or a suboption ending with 0xf0
      will be written to the FIFO, except that as a special case a connect or a
      hangup will be indicated by 0xff followed by 0x01 or 0x00 respectively (i.e.
      two bytes only).
    *)
    procedure NoticeOption(option: integer);

    (* Register an NVT control expansion in the range 241..249.
    *)
    procedure ExpandControl(control: integer; const expand: AnsiString);

    (* Register an NVT control callback in the range 241..249.
    *)
    procedure RegisterControl(control: integer; proc: TelnetProc);

    (* If supported by the kernel, flush any pending data associated with the
      socket.
    *)
    procedure Sync;

    (* Return a response to e.g. a Telnet option, with optional CRLF termination,
      and with no escaping of embedded IAC characters. Return false if the response
      cannot be sent, e.g. because this is being called in a hangup handler after
      the socket has been shut down.
    *)
    function Respond(const msg: ansistring; eol: boolean= false): boolean; virtual;

    (* Send arbitrary data with escaping of IAC, returning the number of bytes
      actually sent or <0 on error (typically a hangup). Note that the returned
      value might be larger than the number of bytes specified as the parameter.
    *)
    function Send(const buffer; count: longint): longint;

    (* Send arbitrary data with escaping of IAC, returning the number of bytes
      actually sent or <0 on error (typically a hangup). Note that the returned
      value might be larger than the number of bytes specified as the parameter.
    *)
    function Send(const buffer: array of byte; count: longint): longint;

    (* Send arbitrary data with escaping of IAC, returning the number of bytes
      actually sent or <0 on error (typically a hangup). Note that the returned
      value might be larger than the number of bytes specified as the parameter.
    *)
    function Send(const buffer: array of AnsiChar; count: longint): longint;

    (* Send arbitrary data with escaping of IAC, returning false on error
      (typically a hangup).
    *)
    function Send(const line: AnsiString; eol: boolean= false): boolean;

    (* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
      actually read or <0 on error (typically a hangup).
    *)
    function Recv(const buffer; count: longint): longint;

    (* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
      actually read or <0 on error (typically a hangup). The buffer must be at least
      as large as is required to hold the data, and no attempt is made to change its
      length.
    *)
    function Recv(out buffer: array of byte; count: longint): longint;

    (* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
      actually read or <0 on error (typically a hangup). The buffer must be at least
      as large as is required to hold the data, and no attempt is made to change its
      length.
    *)
    function Recv(out buffer: array of AnsiChar; count: longint): longint;

    (* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
      actually read or <0 on error (typically a hangup). The existing content of the
      string is overwritten, and the length is changed to fit the data actually read
      (zero-length on error).
    *)
    function Recv(out line: AnsiString; count: longint; trim: boolean= false): longint;

    (* Return the number of bytes buffered for reception by the background thread,
      noting that this might be subject to the client's LINEMODE option state etc.

       This may be safely called from the main program thread, with the result
      applying to the ReadCharTimeout() or ReadLn() etc. that immediately follows.
    *)
    function CharsAvailable(): integer;

    (* Read a single character, obeying the timeout if this is +ve or waiting
      indefinitely if it is -ve. Return false if nothing is available, noting
      that this might be subject to the client's LINEMODE option state etc.

       Assume that this needs a background thread to handle timeouts reliably,
      rather than relying on polled operation.

       This may be safely called from the main program thread, in the same way as
      ReadLn() and interleaved with WriteLn() etc. However calls to this function
      MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
      best behave unpredictably.
    *)
    function ReadCharTimeout(out c: AnsiChar; mSec: integer= DefaultTimeout): boolean;

    (* Read a single character, obeying the timeout if this is +ve or waiting
      indefinitely if it is -ve. Return zero if nothing is available, noting that
      this might be subject to the client's LINEMODE option state etc.

       Assume that this needs a background thread to handle timeouts reliably,
      rather than relying on polled operation.

       This may be safely called from the main program thread, in the same way as
      ReadLn() and interleaved with WriteLn() etc. However calls to this function
      MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
      best behave unpredictably.
    *)
    function ReadCharTimeout(mSec: integer= DefaultTimeout): AnsiChar;

    (* Read a character at a time, accumulating it into the parameter. Return true
      when a complete line is available, noting that this might be subject to the
      client's LINEMODE option state etc.

      Assume that this really needs a background thread to handle timeouts
     reliably, rather than relying on polled operation.

       This may be safely called from the main program thread, in the same way as
      ReadLn() and interleaved with WriteLn() etc. However calls to this function
      MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
      best behave unpredictably.
    *)
    function ReadLnTimeout(var line: AnsiString; mSec: integer= DefaultTimeout;
                                                mSec2: integer= -1): ReadLnStatus;

    (* Read multiple characters, obeying the timeout if this is +ve or waiting
      indefinitely if it is -ve. Return zero if nothing is available, noting that
      this might be subject to the client's LINEMODE option state etc.

      Assume that this needs a background thread to handle timeouts reliably,
     rather than relying on polled operation.

       This may be safely called from the main program thread, in the same way as
      ReadLn() and interleaved with WriteLn() etc. However calls to this function
      MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
      best behave unpredictably.
    *)
    function ReadCharsTimeout(out buffer: array of AnsiChar; count: integer;
                                                mSec: integer= DefaultTimeout): integer;

    (* Read multiple bytes, obeying the timeout if this is +ve or waiting
      indefinitely if it is -ve. Return zero if nothing is available, noting that
      this might be subject to the client's LINEMODE option state etc.

       Assume that this needs a background thread to handle timeouts reliably,
      rather than relying on polled operation.

       This may be safely called from the main program thread, in the same way as
      ReadLn() and interleaved with WriteLn() etc. However calls to this function
      MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
      best behave unpredictably.
    *)
    function ReadBytesTimeout(out buffer: array of byte; count: integer;
                                                mSec: integer= DefaultTimeout): integer;

    (* This is the port that this end of the link is using, possibly randomised.
    *)
    property PortNumber: integer read fPortNumber;

    (* If true, the link is active.
    *)
    property Online: boolean read fOnline;

    (* Recover a "noticed" option from the FIFO.
    *)
    property Option: AnsiString read getOption;

{$if FPC_FULLVERSION < 030000 }
    (* This doesn't have quite the same semantics as the "official" Finished
      property implemented by FPC v3, but is near enough for test purposes.
    *)
    property Finished: Boolean read GetFinished;
{$endif FPC_FULLVERSION       }
  end;

  (* An exception of this type will occur during any of the application-level INPUT
    and OUTPUT operations if the Telnet connection is lost.
  *)
  ETelnetHangup= class(Exception);

(* If the thread manager hasn't been installed then we can't safely initialise
  TThread etc. Refer to use of CThreads in the main program.
*)
function ThreadManagerInstalled(): boolean;


implementation

uses
  UnixType {$ifdef LCL } , Forms {$endif } ;


(* If the thread manager hasn't been installed then we can't safely initialise
  TThread etc. Refer to use of CThreads in the main program.
*)
function ThreadManagerInstalled(): boolean;

var
  tm: TThreadManager;

begin
  result := false;
  if GetThreadManager(tm{%H-}) then
    result := Assigned(tm.InitManager)
end { ThreadManagerInstalled } ;


constructor TTelnetCommon.Create(CreateSuspended: boolean);

var
  i: integer;

begin
  if ThreadManagerInstalled() then
    inherited Create(CreateSuspended);
  fPortNumber := -1;
  fOnline := false;
  fRunning := false;               (* Might be relying on Poll()                *)
  Threshold := 0;
  fOnAvailable := nil;
  AvailableEventPrimed := false;
  inputBuffer := TByteBuffer.Create;
  for i := -1 to 256 do begin
    registeredOptions[i] := nil;
    noticed[i] := false
  end;
  IacInBand := false;
  TelnetIdle := nil;
  lineState := Reading;
  fOptions := TStringList.Create;
  for i := 241 to 249 do begin
    nvtExpansions[i] := #255 + AnsiChar(i);
    nvtControls[i] := nil
  end;
  Hangups := 0
end { TTelnetCommon.Create } ;


destructor TTelnetCommon.Destroy;

begin
  FreeAndNil(inputBuffer);
  FreeAndNil(fOptions);
  inherited Destroy
end { TTelnetCommon.Destroy } ;


procedure dumpOption({%H-}dir: AnsiChar; const {%H-}message: AnsiString);

{ define DO_DUMPOPTION }

{$ifdef DO_DUMPOPTION }


  function optionName(num: AnsiChar): AnsiString;

  (* From https://www.iana.org/assignments/telnet-options/telnet-options.xhtml  *)
  (* as of December 2022.                                                       *)

  begin
    result := IntToStr(Ord(num)) + ' (';
    case Ord(num) of
        0: result += 'Binary Transmission, [RFC856]';
        1: result += 'Echo, [RFC857]';
        2: result += 'Reconnection, [NIC 15391 of 1973]';
        3: result += 'Suppress Go Ahead, [RFC858]';
        4: result += 'Approx Message Size Negotiation, [NIC 15393 of 1973]';
        5: result += 'Status, [RFC859]';
        6: result += 'Timing Mark, [RFC860]';
        7: result += 'Remote Controlled Trans and Echo, [RFC726]';
        8: result += 'Output Line Width, [NIC 20196 of August 1978]';
        9: result += 'Output Page Size, [NIC 20197 of August 1978]';
       10: result += 'Output Carriage-Return Disposition, [RFC652]';
       11: result += 'Output Horizontal Tab Stops, [RFC653]';
       12: result += 'Output Horizontal Tab Disposition, [RFC654]';
       13: result += 'Output Formfeed Disposition, [RFC655]';
       14: result += 'Output Vertical Tabstops, [RFC656]';
       15: result += 'Output Vertical Tab Disposition, [RFC657]';
       16: result += 'Output Linefeed Disposition, [RFC658]';
       17: result += 'Extended ASCII, [RFC698]';
       18: result += 'Logout, [RFC727]';
       19: result += 'Byte Macro, [RFC735]';
       20: result += 'Data Entry Terminal, [RFC1043], [RFC732]';
       21: result += 'SUPDUP, [RFC736], [RFC734]';
       22: result += 'SUPDUP Output, [RFC749]';
       23: result += 'Send Location, [RFC779]';
       24: result += 'Terminal Type, [RFC1091]';
       25: result += 'End of Record, [RFC885]';
       26: result += 'TACACS User Identification, [RFC927]';
       27: result += 'Output Marking, [RFC933]';
       28: result += 'Terminal Location Number, [RFC946]';
       29: result += 'Telnet 3270 Regime, [RFC1041]';
       30: result += 'X.3 PAD, [RFC1053]';
       31: result += 'Negotiate About Window Size, [RFC1073]';
       32: result += 'Terminal Speed, [RFC1079]';
       33: result += 'Remote Flow Control, [RFC1372]';
       34: result += 'Linemode, [RFC1184]';
       35: result += 'X Display Location, [RFC1096]';
       36: result += 'Environment Option, [RFC1408]';
       37: result += 'Authentication Option, [RFC2941]';
       38: result += 'Encryption Option, [RFC2946]';
       39: result += 'New Environment Option, [RFC1572]';
       40: result += 'TN3270E, [RFC2355]';
       41: result += 'XAUTH, [Rob_Earhart]';
       42: result += 'CHARSET, [RFC2066]';
       43: result += 'Telnet Remote Serial Port (RSP), [Robert_Barnes]';
       44: result += 'Com Port Control Option, [RFC2217]';
       45: result += 'Telnet Suppress Local Echo, [Wirt_Atmar]';
       46: result += 'Telnet Start TLS, [Michael_Boe]';
       47: result += 'KERMIT, [RFC2840]';
       48: result += 'SEND-URL, [David_Croft]';
       49: result += 'FORWARD_X, [Jeffrey_Altman]';
      138: result += 'TELOPT PRAGMA LOGON, [Steve_McGregory]';
      139: result += 'TELOPT SSPI LOGON, [Steve_McGregory]';
      140: result += 'TELOPT PRAGMA HEARTBEAT, [Steve_McGregory]';
      255: result += 'Extended-Options-List, [RFC861]'
    otherwise
    end;
    if result[Length(result)] = '(' then
      SetLength(result, Length(result) - 2)
    else
      result += ')'
  end { optionName } ;


var
  {%H-}i: integer;

begin
  Write(StdErr, dir, ' ');
  if Length(message) >= 1 then
    if message[1] = iac then
      Write(StdErr, 'IAC ')
    else
      Write(StdErr, Ord(message[1]), ' ');
  if Length(message) >= 2 then
    case message[2] of
      IacDo:   Write(StdErr, 'DO ');
      IacDont: Write(StdErr, 'DONT ');
      IacWill: Write(StdErr, 'WILL ');
      IacWont: Write(StdErr, 'WONT ');
      iacSb:   Write(StdErr, 'SB ');
      iacSe:   Write(StdErr, 'SE ');
      iacGA:   Write(StdErr, 'GA ');
      iacEL:   Write(StdErr, 'EL ');
      iacEC:   Write(StdErr, 'EC ');
      iacAYT:  Write(StdErr, 'AYT ');
      iacAO:   Write(StdErr, 'AO ');
      iacIP:   Write(StdErr, 'IP ');
      iacBRK:  Write(StdErr, 'BRK ');
      iacDM:   Write(StdErr, 'DM ');
      iacNOP:  Write(StdErr, 'NOP ')
    otherwise
      Write(StdErr, Ord(message[2]), ' ')
    end;
  if Length(message) >= 3 then
    Write(StdErr, optionName(message[3]), ' ');
  for i := 4 to Length(message) do
    case message[i] of
      iac:   Write(StdErr, 'IAC ');
      iacSe: if message[i - 1] = iac then
               Write(StdErr, 'SE ')
             else
               Write(StdErr, Ord(message[i]), ' ')
    otherwise
      Write(StdErr, Ord(message[i]), ' ')
    end;
  WriteLn(stderr)
{$else                }
  begin
{$endif DO_DUMPOPTION }
end { dumpOption } ;


procedure TTelnetCommon.onAvailableShim;

begin
  if Threshold > 0 then
    fOnAvailable(self, inputBuffer.BytesAvailable)
  else
    fOnAvailable(self, inputBuffer.BytesInFirstLine)
end { TTelnetCommon.onAvailableShim } ;


(* This is a blocking call to read and process a single byte, filtering out
  Telnet IAC sequences etc. Return FALSE on error.

  As a special case to support polled operation, the read may be deferred.
*)
function TTelnetCommon.readAndEnqueue(blocking: TBlocking= [IsBlocking]): boolean;

var
  b: byte;
  i: integer;


  procedure sendTelnetResponse(option: byte);

  (* All fairly obvious stuff, but summarised nicely at                         *)
  (* https://stackoverflow.com/questions/10413963/telnet-iac-command-answering  *)

  var
    b: byte;
    message: AnsiString;

  begin
    message := Iac + iacReq + Char(option);
    if iacReq <> iacSb then begin
      dumpOption('<', message);

(* Simpler case first: we have a complete Telnet option message which can       *)
(* either be passed on to the application code or can be rejected.              *)

      if Assigned(registeredOptions[option]) or noticed[option] then begin
        if Assigned(registeredOptions[option]) then
          registeredOptions[option](self, message);
        if noticed[option] then begin
          fOptions.Append(message);
          if IacInBand then
            inputBuffer.PutByte(Ord(Iac))       (* Blocking                     *)
        end
      end else
// TODO : Evaluate implications of fallback in light of thorough testing.
        if Assigned(registeredOptions[256]) or noticed[256] then begin
          if Assigned(registeredOptions[256]) then
            registeredOptions[256](self, message);
          if noticed[256] then begin
            fOptions.Append(message);
            if IacInBand then
              inputBuffer.PutByte(Ord(Iac))     (* Blocking                     *)
          end
        end else begin
          case iacReq of
            IacDo,
            IacDont: message := Iac + IacDont;
            IacWill,
            IacWont: message := Iac + IacWont;
          otherwise
            exit
          end;
          message += AnsiChar(option);
          Respond(message)
        end
    end else begin

(* More complex case: gather the remainder of a Telnet suboption message and    *)
(* if the application has expressed a willingness to handle the option pass it  *)
(* on. If the application code has not expressed a willingness to handle it     *)
(* then ignore the message, since assuming that it was preceded by a DO or WILL *)
(* the client should already have been warned off by a DONT or WONT response.   *)

      while not ((message[Length(message) - 1] = Iac) and (message[Length(message)] = iacSe)) do
        if getNextByte(b, blocking) then (* Blocking                            *)
          message += AnsiChar(b);
if Terminated then
  exit { (false) } ;
      dumpOption('<', message);
      if Assigned(registeredOptions[option]) or noticed[option] then begin
        if Assigned(registeredOptions[option]) then
          registeredOptions[option](self, message);
        if noticed[option] then begin
          fOptions.Append(message);
          if IacInBand then
            inputBuffer.PutByte(Ord(Iac))       (* Blocking                     *)
        end
      end else begin
// TODO : Evaluate implications of fallback in light of thorough testing.
        if Assigned(registeredOptions[256]) then
          registeredOptions[256](self, message);
        if noticed[256] then begin
          fOptions.Append(message);
          if IacInBand then
            inputBuffer.PutByte(Ord(Iac))       (* Blocking                     *)
        end
      end
    end
  end { sendTelnetResponse } ;


  (* Something has been added to the input buffer. This is not the same as
    testing that lineState is Reading, since that can be the case after a Telnet
    option is processed with no buffer state change.
  *)
  procedure checkThreshold;

  begin
    if Assigned(fOnAvailable) and AvailableEventPrimed then begin
      if Threshold >= 0 then begin
        if inputBuffer.BytesAvailable > Threshold then begin
          AvailableEventPrimed := false;
          if GetCurrentThreadId() = MainThreadId then
            fOnAvailable(self, inputBuffer.BytesAvailable)
          else
            Synchronize(@onAvailableShim) (* Using inputBuffer.BytesAvailable   *)
        end
      end else                          (* Was a line-end just added?           *)
        if (b in [$00, $0a]) and (inputBuffer.LinesAvailable >= Abs(Threshold)) then begin
          AvailableEventPrimed := false;
          if GetCurrentThreadId() = MainThreadId then
            fOnAvailable(self, inputBuffer.BytesInFirstLine)
          else
            Synchronize(@onAvailableShim) (* Using inputBuffer.LinesAvailable   *)
        end
    end
  end { checkThreshold } ;


begin
  result := true;
  if (not getNextByte(b, blocking)) or (IsDeferred in blocking) then (* Blocking *)
    exit;

(* Debugging note: if b is 85 (0x55, ASCII 'U') here it might indicate that     *)
(* getNextByte() has erroneously tried to return a default value even though    *)
(* it's not been successful in reading a byte from the input socket or stdin.   *)

  if b = GetNextByteDefaultValue then ;

  case lineState of
    Reading:     case b of
                   $ff: lineState := SeenIac;   (* Telnet IAC, handle specially *)
                 otherwise              (* Printable characters, accumulate     *)
                   inputBuffer.PutByte(b);      (* Blocking                     *)
                   checkThreshold
                 end;
    SeenIac:     case b of
                   $ff:     begin       (* Repeated/escaped IAC                 *)
                              inputBuffer.PutByte($ff); (* Blocking             *)
                              checkThreshold;
                              lineState := Reading
                            end;
                   $fe,                 (* DONT                                 *)
                   $fd,                 (* DO                                   *)
                   $fc,                 (* WONT                                 *)
                   $fb,                 (* WILL                                 *)
                   $fa:     begin       (* SB (SE should never be seen here)    *)
                              iacReq := AnsiChar(b);
                              lineState := SeenIacReq
                            end;
                   $f1..
                   $f9:     begin
                              for i := 1 to Length(nvtExpansions[b]) do
                                inputBuffer.PutByte(Ord(nvtExpansions[b][i])); (* Blocking *)
                              if Assigned(nvtControls[b]) then
                                nvtControls[b](self, AnsiChar(b));
                              checkThreshold;
                              lineState := Reading
                            end
                 otherwise              (* IAC followed by any other byte       *)
                   inputBuffer.PutByte($ff);    (* Blocking                     *)
                   inputBuffer.PutByte(b);      (* Blocking                     *)
                   checkThreshold;
                   lineState := Reading
                 end;
    SeenIacReq:  begin                  (* Option number                        *)
                   sendTelnetResponse(b);
                   lineState := Reading
                 end
  otherwise
    Assert(false, 'Impossible line state')
  end
end { TTelnetCommon.readAndEnqueue } ;


(* Clear the buffer between the background thread and foreground RTL etc.
*)
procedure TTelnetCommon.clearBuffer;

begin
  lineState := Reading;
  inputBuffer.Clear(false);
  fOptions.Clear
end { TTelnetCommon.clearBuffer } ;


(* Return a noticed option and remove it from the FIFO.
*)
function TTelnetCommon.getOption(): AnsiString;

begin
  if fOptions.Count = 0 then
    result := ''
  else begin
    result := fOptions[0];
    fOptions.Delete(0)
  end
end { TTelnetCommon.getOption } ;


(* The application code's getNextByte() has noticed that the connection state is
  changing. This might have the effect of triggering the [-1] registered option
  etc. Return true if the application function should also return a dummy IAC
  etc.
*)
function TTelnetCommon.connectionStateChanging(connecting: boolean): boolean;

begin
  result := false;
  if connecting then begin
    if Assigned(registeredOptions[-1]) then
      registeredOptions[-1](self, Iac + #$01); (* Signal connection             *)
    if noticed[-1] then begin
      fOptions.Append(Iac + #$01);
      result := IacInBand
    end
  end else begin
    if Assigned(registeredOptions[-1]) then
      registeredOptions[-1](self, Iac + #$00); (* Signal hangup                 *)
    if noticed[-1] then begin
      fOptions.Append(Iac + #$00);
      result := IacInBand
    end
  end
end { TTelnetCommon.connectionStateChanging } ;


(* The enqueued received data exceeeds a certain number of bytes (if threshold
  is > 0) or lines (if threshold < 0). This is called in the context of the
  main thread, and resets AvailableEventPrimed false.
*)
procedure TTelnetCommon.setOnAvailable(onAvailable: AvailableProc);

begin
  fOnAvailable := onAvailable
end { TTelnetCommon.setOnAvailable } ;


{$if FPC_FULLVERSION < 030000 }

(* This doesn't have quite the same semantics as the "official" Finished
  property implemented by FPC v3, but is near enough for test purposes.
*)
function TTelnetCommon.GetFinished(): boolean;

begin
  result := Terminated
end { TTelnetCommon.GetFinished } ;

{$endif FPC_FULLVERSION       }


(* Register a Telnet option in the range -1 through 256 where -1 represents a
  connection/hangup and 256 is a final catch-all.

  The handler should expect either a standard three-byte option negotiation or
  a suboption ending with 0xf0, except that as a special case a hangup will be
  indicated by 0xff followed by 0x01 or 0x00 respectively (i.e. two bytes only).
*)
procedure TTelnetCommon.RegisterOption(telnetOption: integer; proc: TelnetProc);

begin
  if telnetOption < 0 then
    telnetOption := -1;
  if telnetOption <= 256 then
    registeredOptions[telnetOption] := proc
end { TTelnetCommon.RegisterOption } ;


(* Register a Telnet option in the range -1 through 256 where -1 represents a
  connection/hangup and 256 is a final catch-all.

  Either a standard three-byte option negotiation or a suboption ending with 0xf0
  will be written to the FIFO, except that as a special case a connect or a
  hangup will be indicated by 0xff followed by 0x01 or 0x00 respectively (i.e.
  two bytes only).
*)
procedure TTelnetCommon.NoticeOption(option: integer);

begin
  if option < 0 then
    option := -1;
  if option <= 256 then
    noticed[option] := true
end { TTelnetCommon.NoticeOption } ;


(* Register an NVT control expansion in the range 241..249.
*)
procedure TTelnetCommon.ExpandControl(control: integer; const expand: AnsiString);

begin
  if control in [241..249] then
    nvtExpansions[control] := expand
end { TTelnetCommon.ExpandControl } ;


(* Register an NVT control callback in the range 241..249.
*)
procedure TTelnetCommon.RegisterControl(control: integer; proc: TelnetProc);

begin
  if control in [241..249] then
    nvtControls[control] := proc
end { TTelnetCommon.RegisterControl } ;


(* If supported by the kernel, flush any pending data associated with the
  socket.
*)
procedure TTelnetCommon.Sync;

// As suggested at https://stackoverflow.com/questions/855544/is-there-a-way-to-flush-a-posix-socket

var
  flag: cint32;

begin
  flag := 1;
  fpSetSockOpt(fClient, IPPROTO_TCP, TCP_NODELAY, @flag, SizeOf(flag));
  flag := 0;
  fpSetSockOpt(fClient, IPPROTO_TCP, TCP_NODELAY, @flag, SizeOf(flag))
end { TTelnetCommon.Sync } ;


(* Return a response to e.g. a Telnet option, with optional CRLF termination,
  and with no escaping of embedded IAC characters. Return false if the response
  cannot be sent, e.g. because this is being called in a hangup handler after
  the socket has been shut down.
*)
function TTelnetCommon.Respond(const msg: ansistring; eol: boolean= false): boolean;

const
  crlf= #$0d#$0a;

begin
  dumpOption('>', msg);
  try
    if msg <> '' then
      if fpSend(fClient, @msg[1], Length(msg), 0) < 0 then
        inputBuffer.Hungup;             (* Might result in an ETelnetHangup     *)
    if eol then
      if fpSend(fClient, @crlf[1], Length(crlf), 0) < 0 then
        inputBuffer.Hungup;             (* Might result in an ETelnetHangup     *)
    result := true
  except
    on e: ETelnetHangup do
      result := false
    else
      raise
  end
end { TTelnetCommon.Respond } ;


(* Send arbitrary data with escaping of IAC, returning the number of bytes
  actually sent or <0 on error (typically a hangup). Note that the returned
  value might be larger than the number of bytes specified as the parameter.
*)
function TTelnetCommon.SendBuffer(const buffer; count: longint): longint;

type
  tpb= ^byte;


  (* Don't waste time in sendEscaped() (below) allocating storage for extra
    bytes (or making extra calls to fpSend()) unless we really need to.
  *)
  function containsIac(pb: tpb; c: cardinal): boolean; inline;

  begin
    result := false;
    while c > 0 do begin
      if pb^ = $ff then
        exit(true);
      Inc(pb);
      Dec(c)
    end
  end { containsIac } ;


  (* Write application-level data with escaping of any embedded IACs.
  *)
  function sendEscaped(pb: tpb; c: cardinal): integer;

  var
    scratch: AnsiString= '';
    i: integer;

  begin
    for i := 0 to c - 1 do begin
      if pb^ = $ff then
        scratch += Iac + Iac
      else
        scratch += AnsiChar(pb^);
      Inc(pb)
    end;
    i := fpSend(fClient, @scratch[1], Length(scratch), 0);
    if i > c then
      result := c
    else
      result := i
  end { sendEscaped } ;


begin
  if containsIac(tpb(@buffer), count) then
    result := sendEscaped(tpb(@buffer), count)
  else
    result := fpSend(fClient, @buffer, count, 0);
  try
    if result < 0 then begin
      inputBuffer.Hungup;               (* Might result in an ETelnetHangup     *)
      result := 0
    end
  except
    on e: ETelnetHangup do
      result := -1
    else
      raise
  end
end { TTelnetCommon.SendBuffer } ;


(* Send arbitrary data with escaping of IAC, returning the number of bytes
  actually sent or <0 on error (typically a hangup). Note that the returned
  value might be larger than the number of bytes specified as the parameter.
*)
function TTelnetCommon.Send(const buffer; count: longint): longint;

begin
  result := SendBuffer(buffer, count)
end { TTelnetCommon.Send } ;


(* Send arbitrary data with escaping of IAC, returning the number of bytes
  actually sent or <0 on error (typically a hangup). Note that the returned
  value might be larger than the number of bytes specified as the parameter.
*)
function TTelnetCommon.Send(const buffer: array of byte; count: longint): longint;

begin
  result := SendBuffer(buffer[0], count)
end { TTelnetCommon.Send } ;


(* Send arbitrary data with escaping of IAC, returning the number of bytes
  actually sent or <0 on error (typically a hangup). Note that the returned
  value might be larger than the number of bytes specified as the parameter.
*)
function TTelnetCommon.Send(const buffer: array of AnsiChar; count: longint): longint;

begin
  result := SendBuffer(buffer[0], count)
end { TTelnetCommon.Send } ;


(* Send arbitrary data with escaping of IAC, returning false on error
  (typically a hangup).
*)
function TTelnetCommon.Send(const line: AnsiString; eol: boolean= false): boolean;

var
  scratch: AnsiString;
  i: integer= 0;

begin
  try
    if not eol then
      if line <> '' then
        i := SendBuffer(line[1], Length(line)) (* Might result in an ETelnetHangup *)
      else begin end
    else begin
      scratch := line + #$0d#$0a;
      i := SendBuffer(scratch[1], Length(scratch)) (* Might result in an ETelnetHangup *)
    end;
    result := i >= Length(line)
  except
    on e: ETelnetHangup do
      result := false
    else
      raise
  end
end { TTelnetCommon.Send } ;


(* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
  actually read or <0 on error (typically a hangup).
*)
function TTelnetCommon.RecvBuffer(const buffer; count: longint): longint;

type
  tpb= ^byte;

var
  pb: tpb;


  procedure wasteTime; inline;

  begin
    Sleep(1);
    if Assigned(TelnetIdle) then
      TelnetIdle(self)
    else
      if GetCurrentThreadId() = MainThreadId then begin
{$ifdef LCL }
        Application.ProcessMessages
{$endif LCL }
      end
  end { wasteTime } ;


begin
  result := 0;
  pb := tpb(@buffer);
  try
    while (result < count) and not Terminated do begin

(* If the background thread is not running, i.e. the program is relying on      *)
(* polling, the only thing we can do here is return immediately. I'm not sure   *)
(* that this is entirely right, so have tentatively left an assertion here.     *)

//      Assert(fRunning, 'RecvBuffer() etc. might be incompatible with polled operation.');
      while fRunning and (inputBuffer.BytesAvailable = 0) and not Terminated do
        wasteTime;
      pb^ := inputBuffer.GetByte();     (* Might result in an ETelnetHangup     *)
      Inc(pb);
      result += 1
    end
  except
    on e: ETelnetHangup do
      result := -1
    else
      raise
  end
end { TTelnetCommon.RecvBuffer } ;


(* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
  actually read or <0 on error (typically a hangup).
*)
function TTelnetCommon.Recv(const buffer; count: longint): longint;

begin
  result := RecvBuffer(buffer, count)
end { TTelnetCommon.Recv } ;


(* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
  actually read or <0 on error (typically a hangup). The buffer must be at least
  as large as is required to hold the data, and no attempt is made to change its
  length.
*)
function TTelnetCommon.Recv(out buffer: array of byte; count: longint): longint;

begin
  Assert(count <= Length(buffer), 'In TTelnetCommon.Recv(), byte buffer too small');
  result := RecvBuffer(buffer[0], count)
end { TTelnetCommon.Recv } ;


(* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
  actually read or <0 on error (typically a hangup). The buffer must be at least
  as large as is required to hold the data, and no attempt is made to change its
  length.
*)
function TTelnetCommon.Recv(out buffer: array of AnsiChar; count: longint): longint;

begin
  Assert(count <= Length(buffer), 'In TTelnetCommon.Recv(), character buffer too small');
  result := RecvBuffer(buffer[0], count)
end { TTelnetCommon.Recv } ;


(* Receive arbitrary data with de-escaping of IAC, returning the number of bytes
  actually read or <0 on error (typically a hangup). The existing content of the
  string is overwritten, and the length is changed to fit the data actually read
  (zero-length on error).
*)
function TTelnetCommon.Recv(out line: AnsiString; count: longint; trim: boolean= false): longint;

begin
  SetLength(line, count);
  result := RecvBuffer(line[1], count);
  if result > 0 then
    SetLength(line, result)
  else
    line := '';
  while trim and (line <> '') and (line[length(line)] in [#$00, #$0a, #$0d]) do begin
    SetLength(line, Length(line) - 1);
    result -= 1
  end
end { TTelnetCommon.Recv } ;


(* Return the number of bytes buffered for reception by the background thread,
  noting that this might be subject to the client's LINEMODE option state etc.

   This may be safely called from the main program thread, with the result
  applying to the ReadCharTimeout() or ReadLn() etc. that immediately follows.
*)
function TTelnetCommon.CharsAvailable(): integer;

begin
  result := inputBuffer.BytesAvailable
end { TTelnetCommon.CharsAvailable } ;


{$if not declared(GetTickCount64) }

(* This is an extreme fallback for cases where an old compiler is being used
  for testing. This is not expected to result in accurate timing, but should
  be marginally adequate for the purpose intended.
*)
function GetTickCount64(): qword; inline;

begin
{$push }
{$rangechecks off}
{$overflowchecks off }
  result := Round(Now() * MSecsPerDay)
{$pop }
end { GetTickCount64 } ;

{$endif declared                  }


(* Read a single character, obeying the timeout if this is +ve or waiting
  indefinitely if it is -ve. Return false if nothing is available, noting that
  this might be subject to the client's LINEMODE option state etc.

   Assume that this needs a background thread to handle timeouts reliably,
  rather than relying on polled operation.

   This may be safely called from the main program thread, in the same way as
  ReadLn() and interleaved with WriteLn() etc. However calls to this function
  MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
  best behave unpredictably.
*)
function TTelnetCommon.ReadCharTimeout(out c: AnsiChar; mSec: integer= DefaultTimeout): boolean;

var
  started: qword;


  procedure wasteTime; inline;

  begin
    Sleep(1);
    if Assigned(TelnetIdle) then
      TelnetIdle(self)
    else
      if GetCurrentThreadId() = MainThreadId then begin
{$ifdef LCL }
        Application.ProcessMessages
{$endif LCL }
      end
  end { wasteTime } ;


begin
  if mSec < 0 then
    mSec := -1;

(* A significant timeout will always be a problem if the background thread is   *)
(* not running.                                                                 *)

  if not fRunning then begin
    if mSec > TimeoutCutoff then
      Assert(fRunning, 'ReadCharTimeout() with a significant timeout is incompatible with polled operation.');
    Sleep(PauseInLieu)
  end;

(* If the background thread is not running, i.e. the program is relying on      *)
(* polling, the only thing we can do here is return immediately. I'm not sure   *)
(* that this is entirely right, so have tentatively left an assertion here.     *)

//  Assert(fRunning, 'ReadCharTimeout() etc. are incompatible with polled operation.');
  case mSec of
    -1: while fRunning and  (inputBuffer.BytesAvailable = 0) and not Terminated do
          wasteTime;
     0: ;
  otherwise
    started := GetTickCount64();
{$push }
{$rangechecks off}
{$overflowchecks off }
    while fRunning and (inputBuffer.BytesAvailable = 0) and (mSec > GetTickCount64() - started) and not Terminated do
      wasteTime
{$pop }
  end;
  c := #$00;
  result := inputBuffer.BytesAvailable <> 0;
  if result then
    c := AnsiChar(inputBuffer.GetByte())
end { TTelnetCommon.ReadCharTimeout } ;


(* Read a single character, obeying the timeout if this is +ve or waiting
  indefinitely if it is -ve. Return zero if nothing is available, noting that
  this might be subject to the client's LINEMODE option state etc.

   Assume that this needs a background thread to handle timeouts reliably,
  rather than relying on polled operation.

   This may be safely called from the main program thread, in the same way as
  ReadLn() and interleaved with WriteLn() etc. However calls to this function
  MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
  best behave unpredictably.
*)
function TTelnetCommon.ReadCharTimeout(mSec: integer= DefaultTimeout): AnsiChar;

var
  c: AnsiChar;

begin
  if ReadCharTimeout(c, mSec) then
    result := c
  else
    result := #$00
end { TTelnetCommon.ReadCharTimeout } ;


(* Read a character at a time, accumulating it into the parameter. Return true
  when a complete line is available, noting that this might be subject to the
  client's LINEMODE option state etc.

   Assume that this needs a background thread to handle timeouts reliably,
  rather than relying on polled operation.

   This may be safely called from the main program thread, in the same way as
  ReadLn() and interleaved with WriteLn() etc. However calls to this function
  MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
  best behave unpredictably.
*)
function TTelnetCommon.ReadLnTimeout(var line: AnsiString; mSec: integer= DefaultTimeout;
                                                mSec2: integer= -1): ReadLnStatus;

label
  bogusStartOfLine;

var
  c: AnsiChar;
  z: boolean;

begin
  result := ReadLnIncomplete;
  if not fRunning then begin
    if mSec > TimeoutCutoff then
      Assert(fRunning, 'ReadLnTimeout() with a significant timeout is incompatible with polled operation.');
    Sleep(PauseInLieu)
  end;
  if mSec2 < 0 then
    mSec2 := mSec;
  if line = '' then begin

(* Because a line may be terminated by either 0x0d 0x0a or 0x0d 0x00 (see       *)
(* below), either 0x0a or 0x00 at the start of a line should be discarded with  *)
(* no effect on timeout usage; I think I'd be justified in discarding all 0x0a  *)
(* and 0x00 in non-binary mode but this seems like a reasonable compromise.     *)
(* Regrettably, a goto is probably the clearest way of expressing this.         *)

bogusStartOfLine:
    z := ReadCharTimeout(c, mSec);
    if z and (c in [#$00, #$0a]) then
      goto bogusStartOfLine
  end else
    z := ReadCharTimeout(c, mSec2);
  if not z then
    result := ReadLnTimedout
  else
    case c of

(* If the Telnet client is in the default line mode a line received here is     *)
(* terminated 0x0d 0x0a (i.e. CR LF etc.) while if the client has been switched *)
(* to character-by-character mode it is terminated 0x0d 0x00. Assume that none  *)
(* of those characters in isolation is an adequate line ending, but when        *)
(* recognised either of those combinations should be removed from the end of a  *)
(* line.                                                                        *)
(*                                                                              *)
(* For the sake of symmetry, assume that lines being sent from the server to    *)
(* the client have similar termination.                                         *)

      #$0d: if PortNumber < 0 then
              result := ReadLnComplete
            else
              line += c;
      #$00,
      #$0a: if (line <> '') and (line[Length(line)] = #$0d) then begin
              SetLength(line, Length(line) - 1);
              result := ReadLnComplete
            end else
              line += c
    otherwise
      line += c
    end
end { TTelnetCommon.ReadLnTimeout } ;


(* Read multiple characters, obeying the timeout if this is +ve or waiting
  indefinitely if it is -ve. Return zero if nothing is available, noting that
  this might be subject to the client's LINEMODE option state etc.

   Assume that this needs a background thread to handle timeouts reliably,
  rather than relying on polled operation.

   This may be safely called from the main program thread, in the same way as
  ReadLn() and interleaved with WriteLn() etc. However calls to this function
  MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
  best behave unpredictably.
*)
function TTelnetCommon.ReadCharsTimeout(out buffer: array of AnsiChar; count: integer;
                                            mSec: integer= DefaultTimeout): integer;

var
  started: qword;

begin
  result := 0;
  if not fRunning then begin
    if mSec > TimeoutCutoff then
      Assert(fRunning, 'ReadCharsTimeout() with a significant timeout is incompatible with polled operation.');
    Sleep(PauseInLieu)
  end;
  if mSec < 0 then
    while result < count do
      result += Ord(ReadCharTimeout(buffer[result], -1))
  else begin
    started := GetTickCount64();
{$push }
{$rangechecks off}
{$overflowchecks off }
    while (result < count) and (mSec > GetTickCount64() - started) do
      result += Ord(ReadCharTimeout(buffer[result], 1))
{$pop }
  end
end { TTelnetCommon.ReadCharsTimeout } ;


(* Read multiple bytes, obeying the timeout if this is +ve or waiting
  indefinitely if it is -ve. Return zero if nothing is available, noting that
  this might be subject to the client's LINEMODE option state etc.

   Assume that this needs a background thread to handle timeouts reliably,
  rather than relying on polled operation.

   This may be safely called from the main program thread, in the same way as
  ReadLn() and interleaved with WriteLn() etc. However calls to this function
  MUST NOT be interleaved with calls to ReadLn(), any attempt to do so will at
  best behave unpredictably.
*)
function TTelnetCommon.ReadBytesTimeout(out buffer: array of byte; count: integer;
                                            mSec: integer= DefaultTimeout): integer;

var
  started: qword;

begin
  result := 0;
  if not fRunning then begin
    if mSec > TimeoutCutoff then
      Assert(fRunning, 'ReadBytesTimeout() with a significant timeout is incompatible with polled operation.');
    Sleep(PauseInLieu)
  end;
  if mSec < 0 then
    while result < count do
      result += Ord(ReadCharTimeout(AnsiChar(buffer[result]), -1))
  else begin
    started := GetTickCount64();
{$push }
{$rangechecks off}
{$overflowchecks off }
    while (result < count) and (mSec > GetTickCount64() - started) do
      result += Ord(ReadCharTimeout(AnsiChar(buffer[result]), 1))
{$pop }
  end
end { TTelnetCommon.ReadBytesTimeout } ;


end.

