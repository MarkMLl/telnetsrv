

(* This is called if the telnet client disconnected. This should be interpreted
  as a temporary event which does not terminate the program, but does mean that
  any linemode etc. negotiation should be repeated.
*)
procedure hangupHandler(telnet: TTelnetServer; const option: AnsiString);

begin
  if Length(option) > 1 then
    case option[2] of
      #$00: begin
              WriteLn(StdErr, '# Detected Telnet client disconnection');
              telnet.Hangups += 1
            end;
      #$01: if telnet.Hangups = 0 then
              if telnet.ProgressOnStdErr then
                WriteLn(StdErr, '# Detected Telnet client connection from ' + telnet.ClientAddr)
              else
                WriteLn(StdErr, '# Detected Telnet client connection from ' + telnet.ClientAddr,
                                        ', expecting session PIN ', telnet.SessionPin)
            else
              if telnet.ProgressOnStdErr then
                WriteLn(StdErr, '# Detected Telnet client reconnection from ' + telnet.ClientAddr)
              else
                WriteLn(StdErr, '# Detected Telnet client reconnection from ' + telnet.ClientAddr,
                                        ', expecting session PIN ', telnet.SessionPin)
    otherwise
    end
end { hangupHandler } ;


(* As server, we have sent an IAC WILL ECHO in an attempt to prevent the client
  from doing its own echoing in order to keep the menu-oriented UI clean; we
  expect the client to respond to this with an IAC DO ECHO. We do not expect
  the client to send an unsolicited IAC DO ECHO or IAC DONT ECHO, so should
  ignore any we see since responding risks a message loop.
*)
procedure echoHandler(telnet: TTelnetServer; const option: AnsiString);

const
  echo= #1;

begin
  if (option[1] = Iac) and (Length(option) >= 3) and (option[3] = echo) then
    case option[2] of
      IacWill:  telnet.Respond(Iac + IacDo + echo);
      IacWont:  telnet.Respond(Iac + IacDont + echo)
    otherwise
    end
end { echoHandler } ;


(* The client will probably respond to our IAC DO SUPPRESS_GO_AHEAD with an
  IAC WILL SUPPRESS_GO_AHEAD. It might itself send IAC DO SUPPRESS_GO_AHEAD, to
  which we should also respond with IAC WILL SUPPRESS_GO_AHEAD. For symmetry,
  also handle a potential DONT-WONT exchange.
*)
procedure suppressGoAheadHandler(telnet: TTelnetServer; const option: AnsiString);

const
  suppressGoAhead= #3;

begin
  if (option[1] = Iac) and (Length(option) >= 3) and (option[3] = suppressGoAhead) then
    case option[2] of
      IacDo:   telnet.Respond(Iac + IacWill + suppressGoAhead);
      IacDont: telnet.Respond(Iac + IacWont + suppressGoAhead)
    otherwise
    end
end { suppressGoAheadHandler } ;


(* The client has reponded to our IAC DO LINEMODE etc.
*)
procedure linemodeHandler(telnet: TTelnetServer; const option: AnsiString);

const
  linemode= #34;

begin
  if (option[1] = Iac) and (Length(option) >= 3) and (option[3] = linemode) then
    case option[2] of

(* RFC1184 says that a DO should only be sent by the server, so we don't        *)
(* expect to see this. If we do, WONT it as a courtesy to reduce the risk that  *)
(* a badly-written client locks up.                                             *)

      IacDo:   telnet.Respond(Iac + IacWont + linemode);

(* This is the expected response to the DO that this program, styling itself as *)
(* the server, sent earlier. Assume that we don't need to action it since it    *)
(* will be sent immediately preceding a suboption message.                      *)

      IacWill: ;

(* This is the suboption where the client is telling us what line modes it      *)
(* supports. The first suboption byte is what matters and will probably be 0x03 *)
(* by default representing the EDIT and TRAPSIG bits, the remainder is a        *)
(* FORWARDMASK which we can discard. We need to respond with EDIT, TRAPSIG and  *)
(* MODE_ACK set, I'm unsure of the semantics but it's what works.               *)

// TODO : This should be switched by WantLineMode.

      IacSb:   if (Length(option) > 4) { and (option[4] = #0) } then
                 telnet.Respond(Iac + IacSb + linemode + #1 + #$07 + Iac + IacSe)
    otherwise
    end
end { linemodeHandler } ;


