(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

program TelnetTest;

{$mode objfpc}{$H+}

{$define TERMINAL_THREADED   }          (* Enable precisely one of these tests  *)
{ define LINETEST_THREADED   }
{ define STRINGTEST_THREADED }
{ define BYTESTEST_THREADED  }

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, TelnetClient, TelnetCommon, TelnetBuffer, Sockets, Crt, Termio;

var
  telnet: TTelnetClient;
  localEcho: boolean= true;


{$I clientoptionhandlers.inc }


{$ifdef TERMINAL_THREADED }

function testProc(): boolean;

const
  crlf= #$0d#$0a;

var
  ch: AnsiChar;

begin
  result := true;
  try
    while TelAvailable(telnet) > 0 do begin
      if TelRead(telnet, ch, 1) < 1 then
        exit(false);
      Write(ch)
    end;

// TODO : Ignoring implications of linemode etc.

    if KeyPressed() then begin

(* Noting that ReadKey returns a Char, and that ch is defined as an AnsiChar.   *)
(* For Debian 12 ("Bookworm") with KDE targetting GTK2 I see:                   *)
(*                                                                              *)
(*      £ 194 163    0xc2 0xa3                                                  *)
(*      ¬ 194 172    0xc2 0xac                                                  *)
(*                                                                              *)
(* I don't know where that prefix byte is coming from, but the second appears   *)
(* to correspond to CP1252: which is presumably what was selected when the OS   *)
(* was installed.                                                               *)

      ch := ReadKey();
      case ch of
        #$0d: begin
                if localEcho then
                  WriteLn;
                TelWrite(telnet, crlf[1], 2)
              end;
        ' '..
        #$7e: begin
                if localEcho then
                  Write(ch);
                TelWrite(telnet, ch, 1)
              end;

(* Non-ANSI characters, see note above.                                         *)

        #$c2: begin
                ch := ReadKey();
                if localEcho then
                  Write(ch);
                TelWrite(telnet, ch, 1)
              end;

(* I don't like having things grossly out of order, but these are specials for  *)
(* experimentation. An <Esc> will send an NVT "Are You There?" command to which *)
(* TelnetDemo might- if all the bits have been filled in- respond by querying   *)
(* and reporting terminal type using Telnet option 24.                          *)

        #$1b: telnet.Respond(Iac + IacAYT);

(* A function/cursor key etc. should, according to the Crt unit documentation,  *)
(* return 0x00 followed by a scancode. This should work for the cursor keys and *)
(* first ten function keys (except that <End> might be misinterpreted as <Esc>  *)
(* hence triggering an AYT as above), but I suspect that the <Break> key etc.   *)
(* is only available if the keyboard (as distinct from the terminal) has been   *)
(* set to raw mode using the KDSKBMODE ioctl which requires elevated privilege. *)
(* In addition, "fancy" keys and key combinations shouldn't be expected to work *)
(* for any key- for example <SysRq>- which has been preempted by the kernel or  *)
(* desktop environment. See the KDE settings                                    *)
(*                                                                              *)
(*      KDE System Settings -> Shortcuts -> KWin                                *)
(*      KDE System Settings -> Workspace Behaviour Screen Locking               *)
(*                                                                              *)
(* etc. and possibly the contents of the ~/.config/kglobalshortcutsrc and       *)
(* ~/.config/khotkeysrc files.                                                  *)
(*                                                                              *)
(* In any event, there is always a risk that too much fiddling around with this *)
(* stuff will put the desktop environment into a mode from which is it tricky   *)
(* to recover- at least without being able to find a browser window to look up  *)
(* reference material online. As such, it is probably best avoided.             *)

        #$00: begin
                ch := ReadKey();
                WriteLn('|0x00 0x', HexStr(Ord(ch), 2), '|');

              end
      otherwise
      end
    end
  except
    on e: ETelnetHangup do
      result := false
    else
      raise
  end;
end { testProc } ;

{$endif TERMINAL_THREADED }


{$if defined(LINETEST_THREADED) or defined(STRINGTEST_THREADED) }

function testProc(): boolean;

var
  pattern, sent, responded: AnsiString;
  i, j: integer;
  ch: AnsiChar;


  procedure chomp(var s: AnsiString);

  begin
    while (s <> '') and (s[Length(s)] in [#$00, #$0a, #$0d]) do
      SetLength(s, Length(s) - 1)
  end { chomp } ;


begin
  result := true;

(* Assume that the server won't be asking us for a PIN, but that there might be *)
(* an initial line announcing the type of test. Swallow that without making any *)
(* assumptions about how lines are terminated etc.                              *)

  responded := 'x';
  try
    repeat
    until telnet.ReadCharsTimeout(responded[1], 1, 1000) = 0
  except
    on e: ETelnetHangup do
      exit(false)
    else
      raise
  end;

(* Send random-length ANSI strings of up to 80 characters including terminating *)
(* CRLF, echoing locally and testing the response.                              *)

  Randomize;
  while true do
    try
      pattern := '';
      j := Random(78);
      for i := 0 to j do
        pattern += AnsiChar($20 + Random($5f));
{$ifdef LINETEST_THREADED }
      sent := pattern + #$0d#$0a;
{$else                    }
      sent := pattern + #$0d#$00;
{$endif LINETEST_THREADED }
      WriteLn('> ', pattern);
      TelWrite(telnet, sent[1], Length(sent));
      TelSync(telnet);
      responded := '';
      repeat
        case telnet.ReadLnTimeout(responded, 1000) of
          ReadLnIncomplete: ;
          ReadLnComplete:   break
        otherwise
          responded := '';
          break
        end
      until false;
      chomp(responded);
      WriteLn('< ', responded);
      if responded <> pattern then begin
        Write('| ');
        if Length(responded) = Length(pattern) then begin
          for i := 1 to Length(responded) do
            if responded[i] = pattern[i] then
              Write(' ')
            else
              break;
          WriteLn('^')
        end else
          WriteLn('Tx: ', Length(sent), ' Rx: ', Length(responded));
        Flush(OUTPUT);
        exit(false)
      end;
      KeyPressed()                      (* Check for ^C                         *)
    except
      on e: ETelnetHangup do
        exit(false)
      else
        raise
    end
end { testProc } ;

{$endif defined(LINETEST_THREADED) or defined(STRINGTEST_THREADED) }


{$ifdef BYTESTEST_THREADED }

function testProc(): boolean;

type
  byteSet= set of byte;

var
  bytes: array[0..31] of byte;
  sent, responded: AnsiString;
  total: qword= 0;
  i: integer;


function safeByte(exclude: byteset= []): byte;

begin
  repeat
    result := Random(256)
  until not (result in exclude)
end { safeByte } ;


begin
  result := true;

(* Assume that the server won't be asking us for a PIN, but that there might be *)
(* an initial line announcing the type of test. Swallow that without making any *)
(* assumptions about how lines are terminated etc.                              *)

  responded := 'x';
  try
    repeat
    until telnet.ReadCharsTimeout(responded[1], 1, 1000) = 0
  except
    on e: ETelnetHangup do
      exit(false)
    else
      raise
  end;

(* Send fixed-length byte blocks, echoing locally and testing the response.     *)
(* Note potential exclusion of IAC during testing.                              *)

  Randomize;
  while true do
    try
      sent := '';
      for i := 0 to 31 do begin
        bytes[i] := safeByte( { [$ff] } );
        sent += ' ' + HexStr(bytes[i], 2)
      end;
      if TelWrite(telnet, bytes, 32) <> 32 then
        exit(false);
      TelSync(telnet);
      WriteLn(HexStr(total, 8), ' >', sent);
      if TelRead(telnet, bytes, 32) <> 32 then
        exit(false);
      responded := '';
      for i := 0 to 31 do
        responded += ' ' + HexStr(bytes[i], 2);
      WriteLn(HexStr(total, 8), ' <', responded);
      if responded <> sent then begin
        Write(HexStr(total, 8), ' |');
        if Length(responded) = Length(sent) then begin
          for i := 1 to Length(responded) do
            if responded[i] = sent[i] then
              Write(' ')
            else
              break;
          WriteLn('^')
        end else
          WriteLn('Tx: ', Length(sent), ' Rx: ', Length(responded));
        Flush(OUTPUT);
        exit(false)
      end;
      total += 32;
      KeyPressed()                      (* Check for ^C                         *)
    except
      on e: ETelnetHangup do
        exit(false)
      else
        raise
    end
end { testProc } ;

{$endif BYTESTEST_THREADED }


(* We expect the server to send us an IAC DO TERMINALTYPE, to which we are
  expected to respond with an IAC WILL TERMINALTYPE. At some later point the
  server might send us IAC SB TERMINALTYPE SEND ... SE to which we respond IAC
  SB TERMINALTYPE IS ... SE.
*)
procedure terminalTypeHandler(telnet: TTelnetClient; const option: AnsiString);

const
  terminalType= #24;

begin
  if (option[1] = iac) and (Length(option) >= 3) and (option[3] = terminalType) then
    case option[2] of

(* The server has sent a DO, to which we should respond with a WILL. RFCs >=930 *)
(* emphasise that the terminal type must not be sent until requested by a       *)
(* subsequent suboption.                                                        *)

      IacDo:   telnet.Respond(Iac + IacWill + terminalType);
      IacWill: telnet.Respond(Iac + IacDont + terminalType);

(* This is the suboption where the server is asking us for the terminal type.   *)

      iacSb:   if (Length(option) > 4) and (option[4] = #1) then
                 telnet.Respond(Iac + IacSb + terminalType + #0 + 'TELNETTEST' + Iac + IacSe)
    otherwise
    end
end { terminalTypeHandler } ;


begin
  if (ParamCount() > 0) and (Pos('-version', LowerCase(ParamStr(1))) > 0) then begin
    WriteLn(ParamStr(0), ' (no version information)');
    WriteLn;
    Halt
  end;
  if ParamCount() > 0 then
    telnet := TelOpen(1)                (* Use ParamStr(1) and possibly ParamStr(2) *)
  else
    telnet := TelOpen('telnet://localhost:23');
  if telnet <> InvalidTelnetHandle then
    try

(* Register echo and linemode handlers, then start the background thread.       *)

      telnet.RegisterOption(1, @echoHandler);
      telnet.RegisterOption(3, @suppressGoAheadHandler);
      telnet.RegisterOption(34, @linemodeHandler);
      telnet.RegisterOption(24, @terminalTypeHandler);
      if telnet.Run() then

(* We should send a suppress go-ahead here, but can't since it will mess up the *)
(* server's PIN request. Hold off until the server sends one, then tack ours    *)
(* onto the response.                                                           *)

        repeat
          if not testProc() then
            break
        until telnet.Finished           (* Repeat until explicitly terminated   *)
      else
        WriteLn(StdErr, '# Unable to run Telnet client')
    finally
      TelClose(telnet)
    end
end.

