(* Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+ *)
(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit TelnetBuffer;

(* Thread-safe support class for TelnetServer, implementing an extensible       *)
(* circular buffer.                                             MarkMLl.        *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs;

type

  (* The normal buffer starts with some small size and grows no larger than 4K
    so as to be compatible with the restrictions imposed upon the FPC serial
    unit by the Linux kernel. A jumbo buffer can accomodate a 9000 byte jumbo
    Ethernet frame or the maximum-sized Kermit frame (9024 bytes). A bronto
    buffer is subject to an implementation-defined limit, typically (slightly
    larger than) 16K.
  *)
  TBufferLimit= (Normal, Jumbo, Bronto);

  TByteBuffer= class
  strict private
    fBufferLimit: TBufferLimit;
    fCurrentLimit, fBytesAvailable, fMaxRead, lastPut, nextGet, hangups: integer;
    buffer: array of byte;
    critSect: TCriticalSection;

  (* The normal buffer starts with some small size and grows no larger than 4K
    so as to be compatible with the restrictions imposed upon the FPC serial
    unit by the Linux kernel. A jumbo buffer can accomodate a 9000 byte jumbo
    Ethernet frame or the maximum-sized Kermit frame (9024 bytes). A bronto
    buffer is subject to an implementation-defined limit, typically (slightly
    larger than) 16K.

     This will have no effect if the buffer does not autoextend. Attempting to
    change the limit if the buffer is non-empty is a fatal error (this could
    possibly be relaxed to prohibiting limit reduction if non-empty)
  *)
    procedure setBufferLimit(bufferLimit: TBufferLimit);

    (* If the buffer isn't already at its maximum size, extend it by (roughly) the
      Golden Ratio.

      There are two possibilities here:

      * If the "put" point is (immediately) below the "get" point, then move the
        content above that point to the top of the buffer.

      * Otherwise the "put" point must already be at the top of the buffer.
    *)
    procedure extend;

    (* Assume that this is protected by a critical section in the caller.
    *)
    procedure putByteNC(b: byte); inline;

    (* Assume that this is protected by a critical section in the caller.
    *)
    function getByteNC(): byte; inline;

    (* Examine a byte, the parameter must initially be -1 which is first set to
      nextGet and then incremented; this is for the purpose of counting the number
      of lines in the buffer and checking that at least the first is intact. Assume
      that this is protected by a critical section in the caller.
    *)
    function peekByteNC(var getFrom: integer): byte; inline;

  public

    (* The length of the first line, set by LinesAvailable(). This will be
      invalidated to -1 by any read/get operation.
    *)
    BytesInFirstLine: integer;

    (* Add a single byte to the buffer, blocking if there is no space i.e. it has
      already been extended to its maximum permissible size.
    *)
    procedure PutByte(b: byte);

    (* Remove a single byte from the buffer, blocking if none is available.
    *)
    function GetByte(): byte;

    (* Add a number of bytes to the buffer, blocking until they can all be added
      and returning the number committed.
    *)
    function Write(const b; c: cardinal= 1): integer;

    (* Remove a number of bytes from the buffer, blocking until that number is
      available and returning the number removed which cannot exceed the MaxRead
      value.
    *)
    function Read(out b; c: cardinal= 1): integer;

    constructor Create(bufferLimit: TBufferLimit= Normal);
    destructor Destroy;

    (* Clear the content of the buffer and by default reduce it to its minimum size.
    *)
    procedure Clear(minimise: boolean= true);

    (* Higher-level software has seen a session hangup. Interrupt any loops by
      raising an ETelnetHangup which should propagate through ReadLn(INPUT) etc.,
      but do not clear the buffer. If the parameter is false clear any hangup flag,
      assume this must always be done at some point after a session end has been
      signalled.
    *)
    procedure Hungup(sessionEnd: boolean= true);

    (* This is the number of complete lines currently in the buffer, where a line
      is terminated by 0x0d followed by either 0x0a or 0x00. Assume that this sets
      BytesInFirstLine as a side-effect, but that this will be invalidated by any
      subsequent get/read operation.

      As an implementation detail, assume that this might be relatively slow.
    *)
    function LinesAvailable(): integer;

    (* This is the implementation-defined maximum number of bytes which can be read
      in a single operation.
    *)
    property MaxRead: integer read fMaxRead;

    (* This is the number of bytes currently in the buffer.
    *)
    property BytesAvailable: integer read fBytesAvailable;

    (* The normal buffer starts with some small size and grows no larger than 4K
      so as to be compatible with the restrictions imposed upon the FPC serial
      unit by the Linux kernel. A jumbo buffer can accomodate a 9000 byte jumbo
      Ethernet frame or the maximum-sized Kermit frame (9024 bytes). A bronto
      buffer is subject to an implementation-defined limit, typically (slightly
      larger than) 16K.
    *)
    property BufferLimit: TBufferLimit read fBufferLimit write setBufferlimit;
  end;


implementation

uses
  TelnetCommon;

const
  minSize= 19;                          (* Never attempt to set this to 1!!!    *)
  maxSize= 1024;

(* Maximum size will actually be somewhat larger than this, because of the way  *)
(* that the buffer grows according to the Golden Ratio. These were originally   *)
(* class variables but I think that requires FPC 2.4 so testing a Telnet client *)
(* using GTK1 and FPC 2.2.4 failed.                                             *)
(*                                                                              *)
(* Note that because of a Linux kernel quirk serial buffer content can be mis-  *)
(* reported if it exceeds 4Kbytes. Despite the temptation to match the size of  *)
(* an Ethernet "Jumbo Frame" (9000 bytes) or the maximum Kermit packet (9024    *)
(* bytes), in order to provide a compatible API it's probably worth respecting  *)
(* this limit while recognising that there's some exceedingly small chance that *)
(* somebody might try to use this for a transfer protocol such as XMODEM-1K.    *)

  maxBuff= 1024 + 128;                  (* 16 * golden_ratio^9 is roughly 1,200 *)

const
  topLimit= 14;

type
  TBufferLimitArray= array[0..topLimit] of integer;

const
  bufferLimitArray: TBufferLimitArray= (20, 32, 52, 80, 132, 256, 414, 669,
                                        1081, 1746, 2820, 4555, 7358, 11886, 19200);
  normalIndex= 10;
  jumboIndex= 13;
  brontoIndex= topLimit;


constructor TByteBuffer.Create(bufferLimit: TBufferLimit= Normal);

begin
  fBufferLimit := Normal;
  fCurrentLimit := 0; // TODO : Rest of the limit stuff.
  fMaxRead := maxBuff;
  hangups := 0;
  critSect := TCriticalSection.Create;
  Clear
end { TByteBuffer.Create } ;


destructor TByteBuffer.Destroy;

begin
  SetLength(buffer, 0);
  critSect.Free
end { TByteBuffer.Destroy } ;


(* Clear the content of the buffer and by default reduce it to its minimum size.
*)
procedure TByteBuffer.Clear(minimise: boolean= true);

begin
  fBytesAvailable := 0;
  lastPut := -1;
  nextGet := 0;
  BytesInFirstLine := 0;
  if minimise then begin

(* This might reallocate buffer storage so must be in a critical section.       *)

// TODO : Consider reducing the size to some intermediate point rather than minimising.

    critSect.Enter;
    try
      SetLength(buffer, minSize)
    finally
      critSect.Leave
    end
  end
end { TByteBuffer.Clear } ;


(* Higher-level software has seen a session hangup. Interrupt any loops by
  raising an ETelnetHangup which should propagate through ReadLn(INPUT) etc.,
  but do not clear the buffer. If the parameter is false clear any hangup flag,
  assume this must always be done at some point after a session end has been
  signalled.
*)
procedure TByteBuffer.Hungup(sessionEnd: boolean= true);

begin
  if sessionEnd then
    hangups += 1
  else
    hangups := 0
end { TByteBuffer.Hungup } ;


(* This is the number of complete lines currently in the buffer, where a line
  is terminated by 0x0d followed by either 0x0a or 0x00. Assume that this sets
  BytesInFirstLine as a side-effect, but that this will be invalidated by any
  subsequent get/read operation.

  As an implementation detail, assume that this might be relatively slow.
*)
function TByteBuffer.LinesAvailable(): integer;

var
  i: integer;
  peekAt: integer= -1;

begin
  result := 0;
  BytesInFirstLine := -1;
  if BytesAvailable < 2 then
    exit;
  critSect.Enter;
  BytesInFirstLine := 0;
  try

(* Look at every element of the buffered string except for the final one, which *)
(* will only be relevant if the penultimate is 0x0d (CR).                       *)

    for i := 1 to BytesAvailable - 1 do begin
      if result = 0 then
        BytesInFirstLine += 1;
      if peekByteNC(peekAt) = $0d then  (* peekAt might wrap to zero, but will  *)
        if buffer[peekAt] in [$00, $0a] then begin (* still be valid.           *)
          if result = 0 then
            bytesInFirstLine += 1;      (* Must include both terminating bytes  *)
          result += 1
        end
    end;
    if result < 1 then
      BytesInFirstLine := 0
  finally
    critSect.Leave
  end
end { TByteBuffer.LinesAvailable } ;


function asHexAscii(b: byte): AnsiString;

begin
  result := '0x' + HexStr(b, 2);
  case b of
    $20..
    $7e: result += ' "' + AnsiChar(b) + '"'
  otherwise
  end;
end { asHexAscii } ;


(* The normal buffer starts with some small size and grows no larger than 4K
  so as to be compatible with the restrictions imposed upon the FPC serial
  unit by the Linux kernel. A jumbo buffer can accomodate a 9000 byte jumbo
  Ethernet frame or the maximum-sized Kermit frame (9024 bytes). A bronto
  buffer is subject to an implementation-defined limit, typically (slightly
  larger than) 16K.

   This will have no effect if the buffer does not autoextend. Attempting to
  change the limit if the buffer is non-empty is a fatal error (this could
  possibly be relaxed to prohibiting limit reduction if non-empty)
*)
procedure TByteBuffer.setBufferLimit(bufferLimit: TBufferLimit);

begin
  Assert(fBytesAvailable = 0, 'Attempting to change buffer limit when non-empty.');
  if fBytesAvailable = 0 then begin
    fBufferLimit := bufferLimit;
    Clear
  end
end { TByteBuffer.setBufferLimit } ;


{$define AUTOEXTEND }
{ define DEBUG }


(* If the buffer isn't already at its maximum size, extend it by (roughly) the
  Golden Ratio.

  There are two possibilities here:

  * If the "put" point is (immediately) below the "get" point, then move the
    content above that point to the top of the buffer.

  * Otherwise the "put" point must already be at the top of the buffer.
*)
procedure TByteBuffer.extend;

var
  oldLength, newLength, extendedBy, oldGet, newGet, i, j: integer;

begin
  Assert(fBytesAvailable = Length(buffer), 'Extending unfilled buffer.');
  if Length(buffer) >= maxSize then
    exit;                              (* Already at maximum size               *)
  oldLength := Length(buffer);
  newLength := (oldLength * 21) div 13;
  extendedBy := newLength - oldLength;
{$ifdef DEBUG }
    System.WriteLn(StdErr, 'Autoextend from ', oldLength, ' to ', newLength, ' bytes');
{$endif DEBUG }
  SetLength(buffer, newLength);
  if lastPut >= nextGet then
    exit;                              (* Nothing more to do                    *)
  oldGet := nextGet;
  newGet := nextGet + extendedBy;
{$ifdef DEBUG }
  System.WriteLn(StdErr, 'Move ', oldGet, '..', oldLength - 1, ' to ',
                                        newGet, '..', newLength - 1);
{$endif DEBUG }
  for i := oldLength - 1 downto oldGet do begin
    j := i + extendedBy;
{$ifdef DEBUG }
    System.WriteLn(StdErr, i:3, ' [', i, '] ', asHexAscii(buffer[j]), ' -> [', j, ']');
{$endif DEBUG }
    buffer[j] := buffer[i]
  end;
  nextGet := newGet
end { TByteBuffer.extend } ;


(* Assume that this is protected by a critical section in the caller.
*)
procedure TByteBuffer.putByteNC(b: byte); inline;

begin
{$ifdef DEBUG }
  System.WriteLn(StdErr, 'Enter putByteNC(), buffer ', Length(buffer), ' content ', fBytesAvailable);
{$endif DEBUG }
{$ifdef AUTOEXTEND }
  if (fBytesAvailable = Length(buffer)) and (Length(buffer) < maxSize) then
    extend;
{$endif AUTOEXTEND }
  while fBytesAvailable = Length(buffer) do
    Sleep(10);

(* Rationale for order of operations: we don't increment the put position and   *)
(* compare it with the buffer limits until after we have decided whether we     *)
(* need to (and can) extend the buffer because it's full. Hence the initial     *)
(* value of nextPut is -1.                                                      *)

  lastPut += 1;
  if lastPut = Length(buffer) then
    lastPut := 0;
  buffer[lastPut] := b;
  fBytesAvailable += 1;
{$ifdef DEBUG }
  System.Write(StdErr, 'Exit putByteNC(), at ', lastPut, ' put ', asHexAscii(b));
  System.WriteLn(StdErr, ' content ', fBytesAvailable);
  System.Flush(StdErr)
{$endif DEBUG }
end { TByteBuffer.PutByteNC } ;


(* Add a single byte to the buffer, blocking if there is no space i.e. it has
  already been extended to its maximum permissible size.
*)
procedure TByteBuffer.PutByte(b: byte);

begin
  while (fBytesAvailable = Length(buffer))
                        {$ifdef AUTOEXTEND } and (Length(buffer) >= maxSize) {$endif } do
    Sleep(10);
  critSect.Enter;
  try
    putByteNC(b)
  finally
    critSect.Leave
  end
end { TByteBuffer.PutByte } ;


(* Assume that this is protected by a critical section in the caller.
*)
function TByteBuffer.getByteNC(): byte; inline;

begin
{$ifdef DEBUG }
  System.WriteLn(StdErr, 'Enter getByteNC(), buffer ', Length(buffer), ' content ', fBytesAvailable);
{$endif DEBUG }
  result := buffer[nextGet];
{$ifdef DEBUG }
  System.Write(StdErr, 'Exit getByteNC(), at ', nextGet, ' got ', asHexAscii(result));
  System.WriteLn(StdErr, ' content ', fBytesAvailable);
  System.Flush(StdErr);
{$endif DEBUG }
  fBytesAvailable -= 1;
  nextGet += 1;
  if nextGet = Length(buffer) then
    nextGet := 0
end { TByteBuffer.getByteNC } ;


(* Examine a byte, the parameter must initially be -1 which is first set to
  nextGet and then incremented; this is for the purpose of counting the number
  of lines in the buffer and checking that at least the first is intact. Assume
  that this is protected by a critical section in the caller.
*)
function TByteBuffer.peekByteNC(var getFrom: integer): byte; inline;

begin
  if getFrom < 0 then
    getFrom := nextGet;
  result := buffer[getFrom];
  getFrom += 1;
  if getFrom >= Length(buffer) then
    getFrom := 0
end { TByteBuffer.peekByteNC } ;


(* Remove a single byte from the buffer, blocking if none is available.
*)
function TByteBuffer.GetByte(): byte;

begin
  while fBytesAvailable = 0 do
    if hangups = 0 then
      Sleep(10)
    else
      raise ETelnetHangup.Create('Session hangup in TByteBuffer.GetByte()');
  critSect.Enter;
  try
    BytesInFirstLine := -1;
    result := getByteNC();
  finally
    critSect.Leave
  end
end { TByteBuffer.GetByte } ;


(* Add a number of bytes to the buffer, blocking until they can all be added
  and returning the number committed.
*)
function TByteBuffer.Write(const b; c: cardinal= 1): integer;

var
  pb: ^byte;

// TODO : Limited testing. Needs something like XModem or Kermit put through it.
// A good choice would probably be a program that transmitted random strings of
// up to say 1024 + 64 characters with each line terminated CRLF, and expected
// them to be echoed back verbatim. Follow that with e.g. Queens Kermit, see
// http://www.columbia.edu/kermit/archive.html Assume that the main TelnetServer
// unit handles IAC escaping competently.

begin
  pb := @b;
  result := 0;
  while result < c do begin
    while (fBytesAvailable = Length(buffer))
                        {$ifdef AUTOEXTEND } and (Length(buffer) >= maxSize) {$endif } do
      Sleep(10);
    critSect.Enter;
    try
      BytesInFirstLine := -1;
      while (fBytesAvailable < Length(buffer)) and (Length(buffer) < maxSize) do begin
        putByteNC(pb^);                 (* Increments fBytesAvailable           *)
        Inc(pb);
        Inc(result);
        if result = c then
          exit                          (* Via finally, leaving critical section *)
      end

(* The buffer has been filled, but we have more to write so allow it to drain.  *)

    finally
      critSect.Leave
    end
  end
end { TByteBuffer.Write } ;


(* Remove a number of bytes from the buffer, blocking until that number is
  available and returning the number removed which cannot exceed the MaxRead
  value.
*)
function TByteBuffer.Read(out b; c: cardinal= 1): integer;

var
  pb: ^byte;

// TODO : Limited testing. Needs something like XModem or Kermit put through it.
// A good choice would probably be a program that transmitted random strings of
// up to say 1024 + 64 characters with each line terminated CRLF, and expected
// them to be echoed back verbatim. Follow that with e.g. Queens Kermit, see
// http://www.columbia.edu/kermit/archive.html Assume that the main TelnetServer
// unit handles IAC escaping competently.

begin
  if c > MaxRead then
    c := MaxRead;
  pb := @b;
  result := 0;
  while result < c do begin
    while fBytesAvailable = 0 do
      if hangups = 0 then
        Sleep(10)                       (* fBytesAvailable can increment as long *)
      else
        raise ETelnetHangup.Create('Session hangup in TByteBuffer.Read()');
    critSect.Enter;                     (* as we're outside the critical section. *)
    try
      BytesInFirstLine := -1;
      while fBytesAvailable > 0 do begin
        pb^ := getByteNC();             (* Decrements fBytesAvailable           *)
        Inc(pb);
        Inc(result);
        if result = c then
          exit                          (* Via finally, leaving critical section *)
      end

(* The buffer is empty, but we have not yet read as much as we want so allow it *)
(* to refill.                                                                   *)

    finally
      critSect.Leave
    end
  end
end { TByteBuffer.Read } ;


begin
  Assert(bufferLimitArray[normalIndex] < 4096);
  Assert(bufferLimitArray[jumboIndex] >= 9024);
  Assert(bufferLimitArray[brontoIndex] >= 16384)
end.

