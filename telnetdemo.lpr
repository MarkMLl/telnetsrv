(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

program telnetdemo;

(* This is a minimal Telnet demonstration server.                MarkMLl.       *)

{$mode objfpc}{$H+}

(* Below: this must work both without and with an imported thread manager:      *)
(* treat successful operation of both LOGIN_POLLED_BLOCKING and LOGIN_THREADED, *)
(* with the port in both cases being set to -1 (use stdin/stdout) and then some *)
(* port in the range 1024..65535, as minimal prerequisites (i.e. four tests     *)
(* total).                                                                      *)
(*                                                                              *)
(* Also test with the port >= 65536 to ensure than random port selection works  *)
(* correctly, and with either a fixed session PIN in the range 1..9999 or with  *)
(* -1 for a random number.                                                      *)

{$define LOGIN_THREADED }               (* Enable precisely one of these tests  *)
{ define LOGIN_POLLED_BLOCKING }
{ define LOGIN_POLLED_NONBLOCKING }
{ define ECHO_LINE_THREADED }
{ define ECHO_STRING_THREADED }
{ define ECHO_BYTES_THREADED }

{$if defined(LOGIN_POLLED_BLOCKING) or defined(LOGIN_POLLED_NONBLOCKING) }
  {$define LOGIN_POLLED }
{$else }
  {$define USETHREAD }
{$endif }

{$if defined(LOGIN_POLLED_BLOCKING) or defined(LOGIN_THREADED) }
  {$define WANT_PIN }
{$endif }

uses
{$ifdef USETHREAD }
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
{$endif USETHREAD }
  Classes, SysUtils, TelnetServer, TelnetBuffer, TelnetTextRec, TelnetPrivs,
  TelnetCommon, DynamicModule, LibCapDynamic;

{$ifdef LOGIN_POLLED_BLOCKING }
const
  pollBlocks= true;
{$else }
const
  pollBlocks= false;
{$endif LOGIN_POLLED_BLOCKING }

var
  telnetPort: integer= 9965023;         (* >= 65536 is randomised socket        *)
{$ifdef WANT_PIN }
  telnetPin: integer= -1;               (* < 0 is randomised PIN, 0 is no PIN   *)
{$else           }
  telnetPin: integer= 0;
{$endif WANT_PIN }
  telnet: TTelnetServer= nil;
  response: AnsiString;
  wantLinemode: boolean= true;          (* True unless handling password        *)


{$I serveroptionhandlers.inc }


{$ifdef LOGIN_POLLED }

procedure testProc;

begin
  while not telnet.Online do
    telnet.Poll(pollBlocks);
  Write('Polled bogus login: ');
  response := '';
  while telnet.ReadLnTimeout(response) <> ReadLnComplete do
    telnet.Poll(pollBlocks);
  try
    Write('Bogus password: ');
    response := '';

// There are ongoing problems with trying to get timeouts working with polled
// operation. The polls below will have to be non-blocking in all cases, but
// whenever wasteTime is called in TTelnetCommon.ReadCharTimeout() it will loop
// until timed out.

    while telnet.ReadLnTimeout(response) <> ReadLnComplete do
      telnet.Poll(pollBlocks)
  finally
  end;
// One of the CRLFs is suppressed here pending telling the client not to echo.
  Write({ #$0d + #$0a + } #$0d + #$0a);     (* SunOS/Solaris only has one CRLF here *)
  Write('Login incorrect' + #$0d + #$0a);
  telnet.Hangup
end { testProc } ;

{$endif LOGIN_POLLED }


{$ifdef LOGIN_THREADED }


(* Respond to an NVT AYT by asking the client for its terminal type. This
  obviously assumes that handlers have been set up for option 24.
*)
procedure nvtAytHandler(telnet: TTelnetServer; const option: AnsiString); forward;


procedure testProc;

begin
  while not telnet.Online do
    Sleep(100);

{
Telnet (standard client) session to Linux server, ignoring all negotiation:

S: Debian GNU/Linux 6.0\r\n
S: pye-dev-07 login:<sp> Note Linux identifies system here
C: fred                 This echoed character-by-character
C: \r\0
S: \r\nPassword:<sp>
C: fred                 This not echoed by the server
C: \r\0
S: \r\n\r\n             Note Linux has extra line here
S: Login incorrect\r\n
S: pye-dev-07 login:

The issue.net file which has presumably contributed the first line above is
seen to be terminated by a single \n. It's reasonable to assume that if this
were missing the first line would be login: as below, which is from a system
with no /etc/issue file.

Telnet (standard client) session to Solaris 10 server ditto:

S: login:<sp>           Note Solaris does not identify system here
C: fred                 This echoed character-by-character
C: \r\0
S: \r\nPassword:<sp>
C: fred                 This not echoed by the server
C: \r\0
S: \r\n                 Note Solaris has no extra line here
S: Login incorrect\r\n
S: login:<sp>
}

(* Experimentation suggests that the GNU/inetutils client terminates lines with *)
(* /r/n if in line mode (the default state), but /r/0 if not. Hence in order to *)
(* be compatible with both Linux and Solaris we should make sure that the mode  *)
(* is changed before the user prompt, even if echo isn't disabled until later.  *)
(*                                                                              *)
(* HOWEVER, it turns out that the FPC RTL isn't happy with this, so in order to *)
(* get the best cosmetic result (even if this wouldn't look right if inspected  *)
(* by WireShark etc.) it's best to defer the line mode change until later.      *)

  Write('Threaded bogus login: ');
  try                                   (* Note the exception that results from *)
    ReadLn(response);                   (* signalling a hangup to the input     *)
//    if Pos('`', response) > 0 then
//      nvtAytHandler(telnet, '');        (* This for use during development      *)
    WriteLn(StdErr, 'User ID: ', response)
  except                                (* buffer object.                       *)
    on e: ETelnetHangup do
      exit
    else
      raise
  end;
  wantLineMode := false;                (* The work's done in the callback      *)
  if telnetPort >= 0 then begin
    telnet.Respond(#255#253#3);         (* IAC DO SUPPRESS_GO_AHEAD             *)
    telnet.Respond(#255#253#34);        (* IAC DO LINEMODE, note wantLineMode above *)
    telnet.Respond(#255#251#1)          (* IAC WILL ECHO                        *)
  end;
  try
    Write('Bogus password: ');
    response := '';
    repeat
      case telnet.ReadLnTimeout(response, 5000, 2500) of
        ReadLnIncomplete: ;
        ReadLnComplete:   begin
                            WriteLn(StdErr, 'Password discarded');
                            break
                          end

(* In principle, we could potentially see IAC followed by a terminal command    *)
(* code here (i.e. GO AHEAD and so on). There's provision in TelnetCommon for   *)
(* these to be converted to an escape sequence etc. or to raise an internal     *)
(* (not OS) signal.                                                             *)

      otherwise
        Write(' ***** Timeout *****');
        response := '';
        break
      end
    until false
  finally
    wantLineMode := true;               (* The work's done in the callback      *)
    if telnetPort >= 0 then begin
      telnet.Respond(#255#252#1);       (* IAC WONT ECHO                        *)
      telnet.Respond(#255#253#34);      (* IAC DO LINEMODE, note wantLineMode above *)
//      telnet.Respond(#255#254#3)       (* IAC DONT SUPPRESS_GO_AHEAD           *)
    end
  end;
  Write(#$0d + #$0a + #$0d + #$0a);     (* SunOS/Solaris only has one CRLF here *)
  Write('Login incorrect' + #$0d + #$0a);
  telnet.Hangup
end { testProc } ;

{$endif LOGIN_THREADED }


{$ifdef ECHO_LINE_THREADED }

procedure testProc;

begin
  while not telnet.Online do
    Sleep(100);
  Write('Threaded line echo: ');
  if telnetPort >= 0 then begin
    telnet.Respond(#255#253#3);         (* IAC DO SUPPRESS_GO_AHEAD             *)
    telnet.Respond(#255#251#1)          (* IAC WILL ECHO                        *)
  end;
  try
    repeat
      try                               (* Note the exception that results from *)
        ReadLn(response);               (* signalling a hangup to the input     *)
{}        WriteLn(StdErr, '< ', response)
      except                            (* buffer object.                       *)
        on e: ETelnetHangup do
          exit                          (* Via finally block                    *)
        else
          raise
      end;
// TODO : Do Eoln() and Eof() tell us anything useful here?
      if not telnet.Online then
        break;
      WriteLn(response);
{}      WriteLn(StdErr, '> ', response);
      telnet.Sync
    until not telnet.Online
  finally
    if telnetPort >= 0 then begin
      telnet.Respond(#255#252#1);       (* IAC WONT ECHO                        *)
//      telnet.Respond(#255#254#3)        (* IAC DONT SUPPRESS_GO_AHEAD           *)
    end
  end;
  telnet.Hangup
end { testProc } ;

{$endif ECHO_LINE_THREADED }


{$ifdef ECHO_STRING_THREADED }

procedure testProc;

begin
  while not telnet.Online do
    Sleep(100);
  Write('Threaded string echo: ');
  wantLineMode := false;                (* The work's done in the callback      *)
  if telnetPort >= 0 then begin
    telnet.Respond(#255#253#3);         (* IAC DO SUPPRESS_GO_AHEAD             *)
    telnet.Respond(#255#253#34);        (* IAC DO LINEMODE, note wantLineMode above *)
    telnet.Respond(#255#251#1)          (* IAC WILL ECHO                        *)
  end;
  try
    repeat
      try                               (* Note the exception that results from *)
        ReadLn(response);               (* signalling a hangup to the input     *)
{}        WriteLn(StdErr, '< ', response)
      except                            (* buffer object.                       *)
        on e: ETelnetHangup do
          exit                          (* Via finally block                    *)
        else
          raise
      end;
// TODO : Do Eoln() and Eof() tell us anything useful here?
      if not telnet.Online then
        break;
      WriteLn(response);
{}      WriteLn(StdErr, '> ', response);
      telnet.Sync
    until not telnet.Online
  finally
    wantLineMode := true;               (* The work's done in the callback      *)
    if telnetPort >= 0 then begin
      telnet.Respond(#255#252#1);       (* IAC WONT ECHO                        *)
      telnet.Respond(#255#253#34);      (* IAC DO LINEMODE, note wantLineMode above *)
//      telnet.Respond(#255#254#3)        (* IAC DONT SUPPRESS_GO_AHEAD           *)
    end
  end;
  telnet.Hangup
end { testProc } ;

{$endif ECHO_STRING_THREADED }


{$ifdef ECHO_BYTES_THREADED }

procedure testProc;

var
  bytes: array[0..31] of byte;
  total: qword= 0;
  i: integer;

begin
  while not telnet.Online do
    Sleep(100);
  Write('Threaded 32-byte echo: ');
  wantLineMode := false;                (* The work's done in the callback      *)
  if telnetPort >= 0 then begin
    telnet.Respond(#255#253#3);         (* IAC DO SUPPRESS_GO_AHEAD             *)
    telnet.Respond(#255#253#34);        (* IAC DO LINEMODE, note wantLineMode above *)
    telnet.Respond(#255#251#1)          (* IAC WILL ECHO                        *)
  end;
  try
    FillByte(bytes, SizeOf(bytes), $aa);
    repeat
      if telnet.Recv(bytes, 32) <> 32 then
        break;
{}      Write(StdErr, HexStr(total, 8), ' <');
{}      for i := 0 to 31 do
{}        Write(StdErr, ' ', HexStr(bytes[i], 2));
{}      WriteLn(stdErr);

(* *)

      if telnet.Send(bytes, 32) <> 32 then
        break;
{}      Write(StdErr, HexStr(total, 8), ' >');
{}      for i := 0 to 31 do
{}        Write(StdErr, ' ', HexStr(bytes[i], 2));
{}      WriteLn(stdErr);
      total += 32
    until not telnet.Online
  finally
    wantLineMode := true;               (* The work's done in the callback      *)
    if telnetPort >= 0 then begin
      telnet.Respond(#255#252#1);       (* IAC WONT ECHO                        *)
      telnet.Respond(#255#253#34);      (* IAC DO LINEMODE, note wantLineMode above *)
//      telnet.Respond(#255#254#3)        (* IAC DONT SUPPRESS_GO_AHEAD           *)
    end
  end;
  telnet.Hangup
end { testProc } ;

{$endif ECHO_BYTES_THREADED }


(* We expect the client to send us an IAC WILL TERMINALTYPE, to which we are
  expected to respond with an IAC SB TERMINALTYPE SEND IAC SE.
*)
procedure terminalTypeHandler(telnet: TTelnetServer; const option: AnsiString);

const
  terminalType= #24;

begin
  if (option[1] = iac) and (Length(option) >= 3) and (option[3] = terminalType) then
    case option[2] of
      IacDo:   telnet.Respond(Iac + IacWont + terminalType);

(* We have sent a DO, the client has sent a WILL to which we should respond     *)
(* with an SB.                                                                  *)

      IacWill: telnet.Respond(Iac + IacSb + terminalType + #1 + Iac + IacSe);

(* This is the suboption where the client is responding with a terminal type.   *)

      iacSb:   if (Length(option) > 3) and (option[4] = #0) then
                 telnet.Send('|' + Copy(option, 5, Length(option) - 6) + '|')
    otherwise
    end
end { terminalTypeHandler } ;


(* Respond to an NVT AYT by asking the client for its terminal type. This
  obviously assumes that handlers have been set up for option 24.
*)
procedure nvtAytHandler(telnet: TTelnetServer; const option: AnsiString);

begin
  telnet.Respond(Iac + IacDo + #24)
end { nvtAytHandler } ;


begin
  if (ParamCount() > 0) and (Pos('-version', LowerCase(ParamStr(1))) > 0) then begin
    WriteLn(ParamStr(0), ' (no version information).');
    WriteLn;
    Halt
  end;
  if ParamCount() > 0 then
    telnetPort := StrToInt(ParamStr(1));
  if ParamCount() > 1 then
    telnetPin := StrToInt(ParamStr(2));
  if telnetPort = -1 then
    WriteLn(StdErr, '# Starting Telnet daemon on standard I/O')
  else
    WriteLn(StdErr, '# Starting Telnet daemon');
  telnet := TTelnetServer.Create(INPUT, OUTPUT, telnetPort, telnetPin);
  if Assigned(telnet) then
    try
      telnet.RegisterOption(-1, @hangupHandler);
      telnet.RegisterOption(1, @echoHandler);
      telnet.RegisterOption(3, @suppressGoAheadHandler);
      telnet.RegisterOption(34, @linemodeHandler);
      telnet.ExpandControl(Ord(IacAYT), '');
      telnet.RegisterControl(Ord(IacAYT), @nvtAytHandler);
      telnet.RegisterOption(24, @terminalTypeHandler);

(* Use either a background thread or foreground polls. In practice, background  *)
(* threads might be disabled to mimic the structure of a simple command-line    *)
(* program which typically makes import of cthreads conditional on the presence *)
(* of the LCL.                                                                  *)

{$ifdef USETHREAD }
      if telnet.Run() then begin
        if telnetPort >= 0 then
          WriteLn(StdErr, '# Expecting user connection to port ', telnet.PortNumber);
        repeat
          testProc
        until telnet.Finished           (* Repeat until explicitly terminated   *)
      end else
        if telnetPort >= 0 then
          WriteLn(StdErr, '# Unable to run Telnet server on stdin')
        else
          WriteLn(StdErr, '# Unable to run Telnet server on port ', telnet.PortNumber)
{$else            }
      telnet.Poll(pollBlocks);
      if telnetPort >= 0 then
        WriteLn(StdErr, '# Expecting user connection to port ', telnet.PortNumber);
      telnet.Poll(pollBlocks);
      repeat
        testProc
      until telnet.Finished             (* Repeat until explicitly terminated   *)
{$endif USETHREAD }
    finally
      FreeAndNil(telnet)
    end
  else
    WriteLn(StdErr, '# Unable to create Telnet server')
end.

