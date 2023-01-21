

(* We expect the server to send us an IAC WILL ECHO or IAC WONT ECHO, to which
  we are expected to respond with the nominal IAC DO ECHO or IAC DONT ECHO with
  appropriate local state change. We don't expect to see an unsolicited IAC DO
  ECHO or IAC DONT ECHO so can reject both of those with an IAC DONT ECHO in
  the same way that the fallback handler would if no handler were installed.
*)
procedure echoHandler(telnet: TTelnetClient; const option: AnsiString);

const
  echo= #1;

begin
  if (option[1] = iac) and (Length(option) >= 3) and (option[3] = echo) then
    case option[2] of
      IacDo,
      IacDont:  { telnet.Respond(Iac + IacDont + echo) } ;
      IacWill:  begin
                  localEcho := false;
                  telnet.Respond(Iac + IacDo + echo)
                end;
      IacWont:  begin
                  localEcho := true;
                  telnet.Respond(Iac + IacDont + echo)
                end
    otherwise
    end
end { echoHandler } ;


(* The client will probably respond to our IAC DO SUPPRESS_GO_AHEAD with an
  IAC WILL SUPPRESS_GO_AHEAD. It might itself send IAC DO SUPPRESS_GO_AHEAD, to
  which we should also respond with IAC WILL SUPPRESS_GO_AHEAD. For symmetry,
  also handle a potential DONT-WONT exchange.
*)
procedure suppressGoAheadHandler(telnet: TTelnetClient; const option: AnsiString);

const
  suppressGoAhead= #3;

begin
  if (option[1] = Iac) and (Length(option) >= 3) and (option[3] = suppressGoAhead) then
    case option[2] of
      IacDo:   begin
                 telnet.Respond(Iac + IacWill + suppressGoAhead);

(* We can't send a suppress go-ahead request until after the server has         *)
(* progressed beyond any PIN request etc. Since doctrine suggests that this     *)
(* should always be done, it's probably a good place to tack in any further     *)
(* customisation.                                                               *)

                 telnet.Respond(Iac + IacDo + suppressGoAhead)
               end;
      IacDont: telnet.Respond(Iac + IacWont + suppressGoAhead)
    otherwise
    end
end { suppressGoAheadHandler } ;


(* We expect the server to send us an IAC DO LINEMODE, to which we are expected
  to respond with an IAC WILL LINEMODE followed by an IAC SB LINEMODE ... SE.
  After that expect to receive an IAC SB LINEMODE ... SE from the server to
  which we are not expected to respond.
*)
procedure linemodeHandler(telnet: TTelnetClient; const option: AnsiString);

const
  linemode= #34;

var
  inLinemode: boolean= true;

begin
  if (option[1] = iac) and (Length(option) >= 3) and (option[3] = linemode) then
    case option[2] of

(* The server has sent a DO, to which we should respond with a WILL followed by *)
(* a suboption containing the current mode.                                     *)

      IacDo:   begin
                 telnet.Respond(Iac + IacWill + linemode);
                 if inLineMode then
                   telnet.Respond(Iac + IacSb + linemode + #3 + #1 + Iac + IacSe)
                 else
                   telnet.Respond(Iac + IacSb + linemode + #3 + #0 + Iac + IacSe)
               end;

(* RFC1184 says that a DO should only be sent by the server, so we don't        *)
(* expect to see a WILL. If we do, DONT it as a courtesy to reduce the risk     *)
(* that a badly-written client locks up.                                        *)

      IacWill: telnet.Respond(Iac + IacDont + linemode);

(* This is the suboption where the server is telling us what line mode it would *)
(* prefer we were in. The first suboption byte is what matters and will         *)
(* probably be 0x03 by default representing the EDIT and TRAPSIG bits, the      *)
(* remainder is a FORWARDMASK which we can discard.                             *)

      IacSb:   if (Length(option) > 5) { and (option[4] = #1) } then
                 inLineMode := (Ord(option[5]) and $03) <> $00
    otherwise
    end
end { linemodeHandler } ;


