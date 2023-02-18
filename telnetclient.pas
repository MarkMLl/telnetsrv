(* Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+ *)
(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit TelnetClient;

(* Implement a simple execution framework for a Telnet client, which is assumed *)
(* to use the Crt unit or similar for user interaction. For communication with  *)
(* a server this presents an API similar to that of FPC's serial unit, except   *)
(* that the open function returns a TThread descendant which handles all data   *)
(* transfer operations.                                                         *)
(*                                                                              *)
(* If the Telnet connection is broken, the object being used in lieu of a       *)
(* handle continues to exist but becomes unusable: it should be closed (which   *)
(* invalidates it) and if necessary a new one opened (i.e. created, which might *)
(* or might not return a numerically-similar reference).                        *)
(*                                                                              *)
(* Telnet options may be monitored and handled by callbacks, or by insertion of *)
(* an IAC character in the normal data flow. A change of online state may be    *)
(* detected similarly as a special case (nominally, this is option -1).         *)
(*                                                                              *)
(*                                                              MarkMLl.        *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, UnixType, TelnetCommon, TelnetBuffer;

const
  INVALID_SOCKET= TelnetCommon.INVALID_SOCKET;
  Iac= TelnetCommon.Iac;
  IacDo= TelnetCommon.IacDo;
  IacDont= TelnetCommon.IacDont;
  IacWill= TelnetCommon.IacWill;
  IacWont= TelnetCommon.IacWont;
  IacSb=  TelnetCommon.IacSb;
  IacSe= TelnetCommon.IacSe;
  InvalidTelnetHandle= nil;

type
  TTelnetClient = class;
  TelnetProc= procedure(telnet: TTelnetClient; const option: AnsiString);
  AvailableProc= procedure(telnet: TTelnetClient; available: integer);

  TTelnetClient= class(TTelnetCommon)
  strict private
  protected
    procedure SetOnline(goOnline: boolean); override;

    (* Accumulate and enqueue client messages, handling Telnet IACs etc. minimally.
    *)
    procedure Execute; override;

    (* This is a blocking call to read one 8-bit character at a time. Note that
      this does not filter out e.g. Telnet IAC sequences.

      As a special case to support polled operation, the read may be deferred.
    *)
    function getNextByte(out b: byte; blocking: TBlocking= [IsBlocking]): boolean; override;

  public

    (* Alternatively use TelOpen() which takes a URI as parameter.
    *)
    constructor Create(const host: AnsiString= 'localhost'; port: integer= 23);

    (* Alternatively use TelOpen() which takes a URI as parameter.
    *)
    constructor Create(const host: AnsiString= 'localhost'; const port: AnsiString= '23');

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

    (* Start up the background thread etc., generally after all registration has
      been done.
    *)
    function Run(bufferLimit: TBufferLimit= Normal): boolean;

    (* Weakly terminate the session, assume that the bulk of the clearing up will
      be done by getNextByte(). This is not recoverable.
    *)
    procedure Hangup(pause: integer=250); override;

    (* The enqueued received data exceeeds the byte threshold. This is called in the
      context of the background thread, and resets AvailableEventPrimed false.
    *)
    procedure SetOnAvailable(onAvailable: AvailableProc);

    (* The enqueued received data exceeeds the byte threshold. This is called in the
      context of the background thread, and resets AvailableEventPrimed false.
    *)
    property OnAvailable: AvailableProc write SetOnAvailable;
  end;

{ Open the telnet device with the given device name, for example:
    telnet://localhost:telnet... for the default local Telnet server
  Returns "0" if device could not be found }
function TelOpen(const DeviceName: String): TTelnetClient;

{ Open the telnet device with the device name etc. specified at ParamStr(n). }
function TelOpen(const Param: integer= 1): TTelnetClient;

{ Closes a telnet device previously opened with TelOpen. }
procedure TelClose(var telnet: TTelnetClient);

{ Suggest to the kernel that buffered output data should be sent. }
procedure TelSync(telnet: TTelnetClient);

{ Discard all pending input. }
procedure TelFlushInput(telnet: TTelnetClient);

(* Return the number of bytes buffered for reception by the operating system,
  or a -ve result on error. Since ioctl() returns an integer of indeterminate
  length via a pointer this has provision for investigating its actual size by
  presetting the return value to a non-zero bit pattern.
*)
function TelAvailable(telnet: TTelnetClient; defaultResult: longint= 0): longint;

{ Reads a maximum of "Count" bytes of data into the specified buffer.
  Result: Number of bytes read. }
function TelRead(telnet: TTelnetClient; var Buffer; Count: LongInt): LongInt;

{ Tries to write "Count" bytes from "Buffer".
  Result: Number of bytes written. }
function TelWrite(telnet: TTelnetClient; Const Buffer; Count: LongInt): LongInt;

{ This is similar to TelRead() but adds a mSec timeout. Note that this variant
  returns as soon as a single byte is available, or as dictated by the timeout. }
function TelReadTimeout(telnet: TTelnetClient; var Buffer; mSec: LongInt): LongInt;

{ This is similar to TelRead() but adds a mSec timeout. Note that this variant
  attempts to accumulate as many bytes as are available, but does not exceed
  the timeout. Set up a TelIdle callback if using this in a main thread in a
  Lazarus app. }
function TelReadTimeout(telnet: TTelnetClient; var Buffer: array of byte; count, mSec: LongInt): LongInt;

type
  Ip4Address= cuint32;

(* Resolve a hostname or an IP4 dotted-quad address, generally trying to "do the
  right thing".
*)
function LookUpHost4(const hostname: AnsiString): Ip4Address;

(* Look up a service (port) name or return a number unchanged. A negative
  parameter is acceptable here since the TelnetServer unit interprets that to
  mean stdin/stdout, and a port >65535 is acceptable as indicating a random
  port.
*)
function LookUpPort(const portname: AnsiString): integer;


implementation

uses
  Sockets, BaseUnix { , Errors } , Resolve, NetDb;

type
  ETelnetForcedTermination= CLASS(Exception);


constructor TTelnetClient.Create(const host: AnsiString= 'localhost'; port: integer= 23);

var
  sockAddr: TInetSockAddr;
  sz: integer;

begin
  inherited Create(true);
  fClient := fpSocket(PF_INET, SOCK_STREAM, 0);
  FillChar(sockAddr{%H-}, SizeOf(sockAddr), 0);
  sockAddr.sin_family := AF_INET;
  sockAddr.sin_port := hToNS(port);
  sockAddr.sin_addr.s_addr := LookupHost4(host);
  sz := fpConnect(fClient, @sockAddr, SizeOf(sockAddr));
  if sz < 0 then begin
    fClient := INVALID_SOCKET;
    fail
  end
end { TTelnetClient.Create } ;


constructor TTelnetClient.Create(const host: AnsiString= 'localhost'; const port: AnsiString= '23');

begin
  Create(host, LookUpPort(port));
  if (self <> nil) and (fClient = INVALID_SOCKET) then
    fail
end { TTelnetClient.Create } ;


destructor TTelnetClient.Destroy;

begin
  SetOnline(false);
  Terminate;

(* I was initially using                                                        *)
(*                                                                              *)
(*    PThread_kill(Handle, SIGHUP);      Anything stronger than HUP here        *)
(*    WaitFor                            would affect the entire process.       *)
(*                                                                              *)
(* here, but the kill caused major problems in a program which was also hooking *)
(* HUP and the WaitFor was just... well, it didn't work once I moved away from  *)
(* using HUP for non-obvious reasons.                                           *)

  if fClient <> INVALID_SOCKET then begin
    fpShutdown(fClient, 2);
    CloseSocket(fClient)
  end;
  repeat
    Sleep(1)
  until Finished;
  inherited Destroy
end { TTelnetClient.Destroy } ;


procedure TTelnetClient.SetOnline(goOnline: boolean);

begin
  fOnline := goOnline
end { TTelnetClient.SetOnline } ;


(* This is a blocking call to read one 8-bit character at a time. Note that
  this does not filter out e.g. Telnet IAC sequences.

  As a special case to support polled operation, the read may be deferred.
*)
function TTelnetClient.getNextByte(out b: byte; blocking: TBlocking= [IsBlocking]): boolean;

const
  deferredListen: boolean= false;       (* Static variable                      *)
  newConnection: boolean= true;         (* Static variable                      *)

var
  deferral: jmp_buf;

begin
  Assert(IsBlocking in blocking, 'Non-blocking poll not implemented.');
  result := true;
  b := GetNextByteDefaultValue;         (* Printable character eases debugging  *)
  if deferredListen then
    LongJmp(deferral{%H-}, 1);

(* The one situation in which this function doesn't block is if it detects a    *)
(* Telnet hangup, in which case if an in-band notification has been requested   *)
(* it will insert an IAC in the data stream. In that case the reversion to a    *)
(* listening state will be deferred, but is enforced by the LongJmp() above.    *)

  if newConnection then begin
    newConnection := false;
    inputBuffer.Hungup(false);
    SetOnline(true);
    if connectionStateChanging(true) then begin
      deferredListen := true;
      if SetJmp(deferral) = 0 then begin
        b := Ord(Iac);
        exit(true)
      end;
      deferredListen := false
    end;
    result := false
  end else begin
// TODO : No provision for non-blocking poll (as alternative to background thread) here.
    if fpRecv(fClient, @b, 1, 0) < 1 then begin         (* Blocking call        *)
      if Terminated then
        Raise ETelnetForcedTermination.Create('forced Termination while reading');
      SetOnline(false);
      CloseSocket(fClient);
      fClient := INVALID_SOCKET;
      ClearBuffer;
      inputBuffer.Hungup;               (* Might result in an ETelnetHangup     *)
      if connectionStateChanging(false) then begin
        deferredListen := true;
        if SetJmp(deferral) = 0 then begin
          b := Ord(Iac);
          exit(true)
        end;
        deferredListen := false
      end;
      result := false;
      Terminate
    end
  end
end { TTelnetClient.getNextByte } ;


(* Accumulate and enqueue client messages, handling Telnet IACs etc. minimally.
*)
procedure TTelnetClient.Execute;

begin
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
end { TTelnetClient.Execute } ;


(* Register a Telnet option in the range -1 through 256 where -1 represents a
  connection/hangup and 256 is a final catch-all.

  The handler should expect either a standard three-byte option negotiation or
  a suboption ending with 0xf0, except that as a special case a hangup will be
  indicated by 0xff followed by 0x01 or 0x00 respectively (i.e. two bytes only).
*)
procedure TTelnetClient.RegisterOption(telnetOption: integer; proc: TelnetProc);

begin
  inherited RegisterOption(telnetOption, TelnetCommon.TelnetProc(proc))
end { TTelnetClient.RegisterOption } ;


(* Register an NVT control callback in the range 241..249.
*)
procedure TTelnetClient.RegisterControl(control: integer; proc: TelnetProc);

begin
  inherited RegisterControl(control, TelnetCommon.TelnetProc(proc))
end { TTelnetClient.RegisterControl } ;


(* Start up the background thread etc., generally after all registration has
  been done.
*)
function TTelnetClient.Run(bufferLimit: TBufferLimit= Normal): boolean;

// TODO : Buffer size.

begin
  result := true;
  inputBuffer.BufferLimit := bufferLimit;
{$if FPC_FULLVERSION >= 020600 }
  Start;
{$else                         }
  Resume;
{$endif FPC_FULLVERSION        }
  fRunning := true                      (* Not relying on Poll()                *)
end { TTelnetClient.Run } ;


(* Weakly terminate the session, assume that the bulk of the clearing up will
  be done by getNextByte(). This is not recoverable.
*)
procedure TTelnetClient.Hangup(pause: integer=250);

begin
  if pause > 0 then                     (* Allow time for any Telnet option     *)
    Sleep(pause);                       (* handshakes to complete etc.          *)
  SetOnline(false);
  if fClient <> INVALID_SOCKET then
    fpShutdown(fClient, 2)
end { TTelnetClient.Hangup } ;


(* The enqueued received data exceeeds the byte threshold. This is called in the
  context of the background thread, and resets AvailableEventPrimed false.
*)
procedure TTelnetClient.SetOnAvailable(onAvailable: AvailableProc);

begin
  inherited SetOnAvailable(TelnetCommon.AvailableProc(onAvailable))
end { TTelnetClient.SetOnAvailable } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890

(* This API is intentionally similar to that of the serial.pp unit supplied as  *)
(* standard with FPC.                                                           *)


{ Open the telnet device with the given device name, for example:
    telnet://localhost:telnet... for the default local Telnet server
  Returns "0" if device could not be found }
function TelOpen(const DeviceName: String): TTelnetClient;

var
  i: integer;
  scratch: AnsiString;
  host: AnsiString= 'localhost';
  port: AnsiString= '23';

begin
  result := nil;
  scratch := DeviceName;
  while (scratch <> '') and (scratch[Length(scratch)] in [' ', '/', '\']) do
    SetLength(scratch, Length(scratch) - 1);
  if Pos('telnet://', scratch) = 1 then begin
    Delete(scratch, 1, Length('telnet://'));
    scratch := Trim(scratch)
  end;
  if scratch <> '' then begin
    i := Length(scratch);
    while (i > 0) and (scratch[i] <> ':') do
      i -=1;
    if i > 0 then begin
      port := Copy(scratch, i + 1, 32767);
      host := Copy(scratch, 1, i - 1)
    end else
      host := scratch
  end;
  while (host <> '') and (host[Length(host)] in [' ', '/', '\']) do
    SetLength(host, Length(host) - 1);

// TODO : No provision for authentication information.
// RFC 4248 mandates   telnet://<user>:<password>@<host>:<port>/  In the current
// context (i.e. referring to the TelnetServer unit) this suggests that it should
// be possible to specify a PIN on the command line rather than entering it
// interactively.

  result := TTelnetClient.Create(host, port);
  if (result <> nil) and (result.fClient = INVALID_SOCKET) then
    result := InvalidTelnetHandle
end { TelOpen } ;


{ Open the telnet device with the device name etc. specified at ParamStr(n). }
function TelOpen(const Param: integer= 1): TTelnetClient;

var
  host: AnsiString= '127.0.0.1';
  port: AnsiString= '23';

begin
  if ParamCount < param then
    result := InvalidTelnetHandle
  else begin
    host := ParamStr(param);
    if Pos('telnet://', host) = 1 then begin
      Delete(host, 1, Length('telnet://'));
      Host := Trim(host)
    end;
    case Pos(':', host) of
      1:     begin
               port := Copy(host, 2, 32767);
               host := '127.0.0.1'
             end;
      2..
      32767: begin
               port := Copy(host, Pos(':', host) + 1, 32767);
               SetLength(host, Pos(':', host) - 1)
             end
    otherwise                           (* No colon, possible second parameter  *)
      if ParamCount() > param then
        port := ParamStr(param + 1)
    end;
    result := TelOpen('telnet://' + host + ':' + port)
  end
end { TelOpen } ;


{ Closes a telnet device previously opened with TelOpen. }
procedure TelClose(var telnet: TTelnetClient);

begin
  FreeAndNil(telnet)
end { TelClose } ;


{ Suggest to the kernel that buffered output data should be sent. }
procedure TelSync(telnet: TTelnetClient);

begin
  telnet.Sync
end { TelSync } ;


{ Discard all pending input. }
procedure TelFlushInput(telnet: TTelnetClient);

begin
  telnet.ClearBuffer
end { TelFlushInput } ;


(* Return the number of bytes buffered for reception by the operating system,
  or a -ve result on error. Since ioctl() returns an integer of indeterminate
  length via a pointer this has provision for investigating its actual size by
  presetting the return value to a non-zero bit pattern.
*)
function TelAvailable(telnet: TTelnetClient; defaultResult: longint= 0): longint;

begin
  result := telnet.InputBuffer.BytesAvailable
end { TelAvailable } ;


{ Reads a maximum of "Count" bytes of data into the specified buffer.
  Result: Number of bytes read. }
function TelRead(telnet: TTelnetClient; var Buffer; Count: LongInt): LongInt;

begin
  result := telnet.InputBuffer.Read(buffer, count)
end { TelRead } ;


{ Tries to write "Count" bytes from "Buffer".
  Result: Number of bytes written. }
function TelWrite(telnet: TTelnetClient; Const Buffer; Count: LongInt): LongInt;

begin
  result := telnet.Send(byte(buffer), count) (* Might result in an ETelnetHangup *)
end { TelWrite } ;


{ This is similar to TelRead() but adds a mSec timeout. Note that this variant
  returns as soon as a single byte is available, or as dictated by the timeout. }
function TelReadTimeout(telnet: TTelnetClient; var Buffer; mSec: LongInt): LongInt;

begin
  if telnet.ReadCharTimeout(AnsiChar(buffer), mSec) then
    result := 1
  else
    result := 0
end { TelReadTimeout } ;


{ This is similar to TelRead() but adds a mSec timeout. Note that this variant
  attempts to accumulate as many bytes as are available, but does not exceed
  the timeout. Set up a TelIdle callback if using this in a main thread in a
  Lazarus app. }
function TelReadTimeout(telnet: TTelnetClient; var Buffer: array of byte; count, mSec: LongInt): LongInt;

begin
  result := telnet.ReadBytesTimeout(buffer, count, mSec)
end { TelReadTimeout } ;


  //############################################################################
 //      1         2         3         4         5         6         7         8
// 45678901234567890123456789012345678901234567890123456789012345678901234567890

(* This is old code borrowed from List3264. There's probably better ways of     *)
(* doing it- which I will investigate presently- but as of late 2022 FPC's      *)
(* fcl-net package is totally devoid of documentation.                          *)


TYPE    TIpMath= cuint32;

(* The above type is whatever turns out to be most appropriate for      *)
(* logical operations on IP (v4) addresses.                             *)


procedure debugMsg(const s: AnsiString);

begin
//  WriteLn(StdErr, s)
end { debugMsg } ;


FUNCTION inet_ntoa(ipAddr: In_addr): STRING;

(* Emulate the corresponding Winsock function. Implementation is      *)
(* inefficient but avoids assignment of large numbers (>= $80000000)  *)
(* to 32-bit variables.                                               *)

VAR     ip32: TIpMath;                  (* Bytes in host order        *)

BEGIN
  ip32:= NtoHL(ipAddr.S_addr);
  RESULT:= IntToStr((ip32 SHR 24) MOD 256) + '.';
  RESULT:= RESULT + IntToStr((ip32 SHR 16) MOD 256) + '.';
  RESULT:= RESULT + IntToStr((ip32 SHR 8) MOD 256) + '.';
  RESULT:= RESULT + IntToStr(ip32 MOD 256)
END { Inet_ntoa } ;


VAR     ipAddressCount: INTEGER;
        ipAddressAlternative: TIpMath;


(* Resolve a hostname or an IP4 dotted-quad address, generally trying to "do the
  right thing".
*)
function LookUpHost4(const hostname: AnsiString): Ip4Address;

(* Hide unpleasantness. Result is in network order, resolves both names *)
(* and IP addresses.                                                    *)

VAR
  resolver: THostResolver;


  FUNCTION numericResolve(CONST hostname: STRING; VAR addr: TIpMath): BOOLEAN;

  (* Convert the parameter to a network-ordered IP address if possible, *)
  (* returning TRUE. On failure return FALSE, the address is undefined  *)
  (* in this case.                                                      *)

  VAR   left, right: STRING;
        dot, temp: INTEGER;

  BEGIN
    RESULT:= FALSE;
    addr:= 0;
    right:= Trim(hostname) + '.';
    dot:= Pos('.', right);
    WHILE dot > 0 DO BEGIN
      left:= right;
      SetLength(left, dot - 1);
      Delete(right, 1, dot);
      IF (Length(left) < 1) OR (Length(left) > 3) THEN
        EXIT;
      TRY
        temp:= StrToInt(left) MOD 256
      EXCEPT
        EXIT
      END;
      addr:= (addr SHL 8) OR temp;
      dot:= Pos('.', right)
    END;
    RESULT:= right = '';
    addr:= HtoNL(addr)
  END { numericResolve } ;


BEGIN
  RESULT:= 0;
  ipAddressCount:= 0;
  ipAddressAlternative:= 0;

(* This is straight from testrhre.pp. I really do not like this idiom at  *)
(* all since it makes it impossible to see whether a failure is because   *)
(* Create() has returned NIL or something's gone wrong later.             *)

//  With THostResolver.Create(Nil) do
//    try
//      If AddressLookup(hostname) then
//        RESULT:= Addresses[0].S_addr
//    finally
//      Free
//    end

  resolver:= THostResolver.Create(Nil);
  try
    IF resolver = NIL THEN
      debugMsg('THostResolver.Create() -> NIL');
    TRY

(* Linux resolver.AddressLookup() doesn't appear to like converting       *)
(* specials, in particular broadcast addresses.                           *)

      IF numericResolve(hostname, RESULT) THEN BEGIN
        ipAddressCount:= 1;
        EXIT
      END;

(* Using WinSock resolver.NameLookup() appears to resolve both names    *)
(* and IP addresses, under Linux (and probably other unices) only names.*)
(* As a general point try the numeric form first as being the faster    *)
(* and easier to reject and then try a name lookup.                     *)

(* Note that some versions of resolve.pas incorrectly return addresses  *)
(* in network rather than host order.                                   *)

      If resolver.AddressLookup(hostname) then BEGIN
        ipAddressCount:= resolver.AddressCount;
        IF resolver.AddressCount < 1 THEN
          debugMsg('resolver.AddressCount < 1');
(*$IFNDEF HAS_NETORDERRESOLVER *)
        debugMsg('resolver.Addresses[0] = ' + inet_ntoa(in_addr(HtoNL(resolver.Addresses[0].S_addr))));
        RESULT:= TIpMath(HtoNL(resolver.Addresses[0].S_addr));
        IF ipAddressCount > 1 THEN
          ipAddressAlternative:= TIpMath(HtoNL(resolver.Addresses[Random(ipAddressCount - 1) + 1].S_addr));
(*$ELSE                        *)
        debugMsg('resolver.Addresses[0] = ' + inet_ntoa(resolver.Addresses[0]));
        RESULT:= TIpMath(resolver.Addresses[0].S_addr);
        IF ipAddressCount > 1 THEN
          ipAddressAlternative:= TIpMath(resolver.Addresses[Random(ipAddressCount - 1) + 1].S_addr);
(*$ENDIF                       *)
        EXIT
      END ELSE
        debugMsg('resolver.AddressLookup(address) -> FALSE');
      If resolver.NameLookup(hostname) then BEGIN
        ipAddressCount:= resolver.AddressCount;
        IF resolver.AddressCount < 1 THEN
          debugMsg('resolver.AddressCount < 1');
(*$IFNDEF HAS_NETORDERRESOLVER *)
        debugMsg('resolver.Addresses[0] = ' + inet_ntoa(in_addr(HtoNL(resolver.Addresses[0].S_addr))));
        RESULT:= TIpMath(HtoNL(resolver.Addresses[0].S_addr));
        IF ipAddressCount > 1 THEN
          ipAddressAlternative:= TIpMath(HtoNL(resolver.Addresses[Random(ipAddressCount - 1) + 1].S_addr))
(*$ELSE                        *)
        debugMsg('resolver.Addresses[0] = ' + inet_ntoa(resolver.Addresses[0]));
        RESULT:= TIpMath(resolver.Addresses[0].S_addr);
        IF ipAddressCount > 1 THEN
          ipAddressAlternative:= TIpMath(resolver.Addresses[Random(ipAddressCount - 1) + 1].S_addr);
(*$ENDIF                       *)
      END ELSE
        debugMsg('resolver.NameLookup(name) -> FALSE')
    EXCEPT
      ipAddressCount:= 0
    END
  finally
    resolver.Free
  end
END { LookUpHost4 } ;


(* Look up a service (port) name or return a number unchanged. A negative
  parameter is acceptable here since the TelnetServer unit interprets that to
  mean stdin/stdout, and a port >65535 is acceptable as indicating a random
  port.
*)
function LookUpPort(const portname: AnsiString): integer;

var
  E: TServiceEntry;

begin
  if not TryStrToInt(portname, result) then begin
    result := 0;
    if GetServiceByName(portName, 'tcp', E) then
      result := E.port
  end
end { LookUpPort } ;


end.

