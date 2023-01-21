(* Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+ *)
(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit TelnetTextRec;

(* Support routines for TelnetServer, handling TextRec manipulation.            *)
(*                                                              MarkMLl.        *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, TelnetServer;

(* Check that the structure of a TextRec, intended to be opaque by the FPC
  developers, is more or less what we'd expect. Note that this is NOT FOOLPROOF.
*)
function TextRecValid(var t: Text; testHandle: boolean= true;
                                        testBuffer: boolean= false): boolean;

(* Bind the INPUT and OUTPUT text devices to facilities provided by a
  TelnetServer instance.
*)
procedure BindTextRecs(server: TTelnetServer; var input, output: text;
                                        closeHandles: boolean; port: integer);


implementation

uses
  BaseUnix;


(* Check that the structure of a TextRec, intended to be opaque by the FPC
  developers, is more or less what we'd expect. Note that this is NOT FOOLPROOF.
*)
function TextRecValid(var t: Text; testHandle: boolean= true;
                                        testBuffer: boolean= false): boolean;

var
  a, b, c, d: ptruint;

begin
  result := false;

(* If we're not actually being asked to test this then return true.             *)

  if not Assigned(@t) then
    exit(true);

(* If the parameter is INPUT or OUTPUT then we can reasonably test the mode and *)
(* possibly handle.                                                             *)

  if @t = @INPUT then begin
    if TextRec(t).mode <> fmInput then
      exit;
    if testHandle and (TextRec(t).Handle <> 0) then
      exit
  end;
  if @t = @OUTPUT then begin
    if TextRec(t).mode <> fmOutput then
      exit;
    if testHandle and (TextRec(t).Handle <> 1) then
      exit
  end;
  if testBuffer then begin
    a := {%H-}ptruint(TextRec(t).bufPtr);    (* Easily accessible to debugger        *)
    b := {%H-}ptruint(@(TextRec(t).buffer));
    if a <> b then
      exit
  end;

(* Test whether the relative position of fields near the start and end of the   *)
(* TextRec type are consistent with our understanding of intervening fields.    *)

  a := {%H-}ptruint(@(TextRec(t).buffer));
  b := {%H-}ptruint(@(TextRec(t).bufPtr));
  c := a - b;

{ define DUMP_TEXTREC }
{$ifdef DUMP_TEXTREC }
  with TextRec(t) do begin
    d := ptruint(@bufptr);
    WriteLn(ERROUTPUT, (d - b):5, ' ', HexStr(d, 16), ' ', HexStr(ptruint(bufptr), 16));
    d := ptruint(@openfunc);
    WriteLn(ERROUTPUT, (d - b):5, ' ', HexStr(d, 16));
    d := ptruint(@inoutfunc);
    WriteLn(ERROUTPUT, (d - b):5, ' ', HexStr(d, 16));
    d := ptruint(@flushfunc);
    WriteLn(ERROUTPUT, (d - b):5, ' ', HexStr(d, 16));
    d := ptruint(@closefunc);
    WriteLn(ERROUTPUT, (d - b):5, ' ', HexStr(d, 16));
    d := ptruint(@userdata);
    WriteLn(ERROUTPUT, (d - b):5, ' ', HexStr(d, 16));
    d := ptruint(@name);
    WriteLn(ERROUTPUT, (d - b):5, ' ', HexStr(d, 16));
    d := ptruint(@LineEnd);
    WriteLn(ERROUTPUT, (d - b):5, ' ', HexStr(d, 16));
    d := ptruint(@buffer);
    WriteLn(ERROUTPUT, (d - b):5, ' ', HexStr(d, 16));
    WriteLn(ERROUTPUT)
  end;
{$endif DUMP_TEXTREC }

(* If we believe we have a good understanding of the fields between the buffer  *)
(* pointer and the buffer itself, then it is likely that the overall data       *)
(* structure- and in particular the code pointers- behaves much as we expect.   *)
(*                                                                              *)
(* I remain concerned that there's isn't a robust definition of those code      *)
(* pointers. As of FPC 3.2.2 text.inc implies that each refers to a procedure   *)
(* taking a TextRec as parameter with data passed in the referenced buffer.     *)
(*                                                                              *)
(* Below derived from FPC 3.2.2 textrec.inc.                                    *)

  d := SizeOf(Pointer); // bufptr
  d += 4 * SizeOf(Pointer); // codepointers
  d += 32; // userdata
{$if declared(TFileTextRecChar) }
  d += textrecnamelength * SizeOf(TFileTextRecChar); // name
{$else                          }
  d += textrecnamelength; // name
{$endif declared                }
  d += 1 + 3; // lineend
  result := c = d                       (* Good place for a breakpoint          *)
end { TextRecValid } ;


(* Non-object shim to allow an RTL TextRec to access a thread supporting a
  Telnet socket etc.
*)
procedure textBufferFromPort(var t: TextRec);

var
  s: TTelnetServer;

begin
  Move(t.UserData[1], s{%H-}, SizeOf(ptruint));
  s.BufferFromPort
end { textBufferFromPort } ;


(* Non-object shim to allow an RTL TextRec to access a thread supporting a
  Telnet socket etc.
*)
procedure textBufferToPort(var t: TextRec);

var
  s: TTelnetServer;

begin
  Move(t.UserData[1], s{%H-}, SizeOf(ptruint));
  s.BufferToPort
end { textBufferToPort } ;


(* Non-object shim to allow an RTL TextRec to access a thread supporting a
  Telnet socket etc.
*)
procedure textFlushToPort(var t: TextRec);

var
  s: TTelnetServer;

begin
  Move(t.UserData[1], s{%H-}, SizeOf(ptruint));
  s.FlushToPort
end { textFlushToPort } ;


(* Non-object shim to allow an RTL TextRec to access a thread supporting a
  Telnet socket etc.

   This is specifically for operations that we would not expect to be used, but
  which might be called if a version of the RTL that we have not inspected does
  something unexpected. It's probably best to assume that the assertion is
  useful to alert us to a problem, but that the most valuable information will
  be found in a debugger backtrace.
*)
procedure neverCall(var t: TextRec);

begin
  Assert(false, 'Unexpected function call relating to ' + t.name + '.')
end { neverCall } ;


(* Bind the INPUT and OUTPUT text devices to facilities provided by a
  TelnetServer instance.
*)
procedure BindTextRecs(server: TTelnetServer; var input, output: text;
                                        closeHandles: boolean; port: integer);

(* We need to accommodate the possibility that one or both of the standard text *)
(* devices is not to be redirected, which includes the case where multiple      *)
(* telnet servers are being started. Assume that an attempt to start multiple   *)
(* servers redirecting a text device more than once will have an indeterminate  *)
(* result.                                                                      *)
(*                                                                              *)
(* Assume that the TextRec data structures were set up by OpenStdIO() in        *)
(* text.inc and that this defines what fields we need to overwrite.             *)

begin
  if Assigned(@input) then
    with TextRec(input) do begin
      if name = '' then
        name := 'handle ' + inttostr(Handle);
      if closeHandles then
        fpClose(Handle);
      Handle := UnusedHandle;           (* No longer used by this text device   *)
      if port < 0 then
        name := 'Handle 0 (was ' + name + ')' (* Name saved for debugging       *)
      else
        name := '0.0.0.0:' + IntToStr(port);
      FillByte(UserData, SizeOf(UserData), 0);
      Move(ptruint(server), UserData[1], SizeOf(ptruint));
      OpenFunc := @neverCall;
      Closefunc := @neverCall;
      InOutFunc := @textBufferFromPort;
      FlushFunc := @neverCall
    end;
  if Assigned(@output) then
    with TextRec(output) do begin
      if name = '' then
        name := 'handle ' + inttostr(Handle);
      if closeHandles then
        fpClose(Handle);
      Handle := UnusedHandle;           (* No longer used by this text device   *)
      if port < 0 then
        name := 'Handle 1 (was ' + name + ')' (* Name saved for debugging       *)
      else
        name := '0.0.0.0:' + IntToStr(port);
      FillByte(UserData, SizeOf(UserData), 0);
      Move(ptruint(server), UserData[1], SizeOf(ptruint));
      OpenFunc := @neverCall;
      Closefunc := @neverCall;
      InOutFunc := @textBufferToPort;
      FlushFunc := @textFlushToPort
    end
end { BindTextRecs } ;


end.

