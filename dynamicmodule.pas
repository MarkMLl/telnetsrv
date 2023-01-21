unit DynamicModule;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  (* Contributed by kodekraft@cix (Martin Halliday), possibly derived from a NAG
    interface library. Minor reformatting and port to Lazarus MarkMLl.
  *)
  TDynamicModule= class
    (* The libraries are dynamically linked via a class, always return true even
      if an attempt to open them is expected to fail.
    *)
    class function IsDynamic(): boolean; inline;
  strict private
    FModuleHandle: THandle;
    FAlreadyInMemory: BOOLEAN;
    FModuleName: string;
    FLoadError: string;
    FLastError: string;
    (* Case-sensitivity depends on the operating system and filesystem.
    *)
    procedure SetModuleName(const Value: string);
    function GetModuleHandle(): THandle;
    procedure UnloadModule;
  public
    (* Case-sensitivity depends on the operating system and filesystem.
    *)
    constructor Create(const ModuleName: string);
    destructor Destroy; override;
    procedure LoadModule;
    (* Note that the name of the entry point is case-sensitive.
    *)
    procedure LoadRoutine(var Routine: Pointer; const Name: string);
    (* This is an overloaded addition to Martin's original code. It can result in
      neater code but is much less efficient since it can't check whether it has
      already been called.
    *)
    function LoadRoutine(const Name: string): Pointer;
    function ModuleExists(): Boolean;
    (* Note that the name of the entry point is case-sensitive.
    *)
    function RoutineExists(const Name: string): Boolean;
    property ModuleName: string read FModuleName write SetModuleName;
    property ModuleHandle: THandle read GetModuleHandle;
    property ModuleInMemory: boolean read FAlreadyInMemory;
    property LoadError: string read FLoadError;
    property LastError: string read FLastError;
  end;

implementation

uses
  DynLibs
{$if not declared(GetLoadErrorStr) }
  {$ifdef UNIX }
        , dl
  {$else       }
        , windows
  {$endif UNIX }
{$endif                            }
  ;

type      DynamicModuleException= class(Exception);


{$if not declared(GetLoadErrorStr) }

function GetLoadErrorStr: string;

var     rc: word;

begin
{$ifdef UNIX }
  result := dl.dlerror
{$else       }
  rc := Windows.GetLastError;
  try
    result := Trim(SysErrorMessage(rc));
    if result = '' then
      result := 'Operating system error 0x' + IntToHex(rc, 8) + ' (no descriptive text)'
  except
    result := 'Operating system error 0x' + IntToHex(rc, 8) + ' (error getting descriptive text)'
  end
{$endif UNIX }
end { GetLoadErrorStr } ;

{$endif                            }


(* The ALSA libraries are dynamically linked via a class, always return true even
  if an attempt to open them is expected to fail.
*)
class function TDynamicModule.IsDynamic(): boolean; inline;

begin
  IsDynamic := true
end { TDynamicModule.IsDynamic } ;


(* Case-sensitivity depends on the operating system and filesystem.
*)
procedure TDynamicModule.SetModuleName(const Value: string);

begin
  UnloadModule;
  FModuleName := Value
end { TDynamicModule.SetModuleName } ;


function TDynamicModule.GetModuleHandle(): THandle;

begin
  LoadModule;
  Result := FModuleHandle;
end { TDynamicModule.GetModuleHandle } ;


procedure TDynamicModule.UnloadModule;

begin
  if (FModuleHandle <> NilHandle) AND NOT FAlreadyInMemory then begin
    FreeLibrary(FModuleHandle);
    FLastError := GetLoadErrorStr
  end;
  FModuleHandle := NilHandle;
  FAlreadyInMemory:= FALSE
end { TDynamicModule.UnloadModule } ;


(* Case-sensitivity depends on the operating system and filesystem.
*)
constructor TDynamicModule.Create(const ModuleName: string);

begin
  inherited Create;
  FModuleHandle := NilHandle;           (* Be absolutely explicit about these   *)
  FAlreadyInMemory:= FALSE;             (* since used for presence testing.     *)
  FModuleName := ModuleName;
  FLoadError := '[Undefined or not implemented]';
  FLastError := FLoadError
end { TDynamicModule.Create } ;


destructor TDynamicModule.Destroy;

begin
  UnloadModule;
  inherited
end { TDynamicModule.Destroy } ;


procedure TDynamicModule.LoadModule;

var     revisedFilename: string;


  function readSecondLine(const name: string): string;

  var   txt: TEXT;

  begin
    Assign(txt, name);
    Reset(txt);
    repeat
      ReadLn(txt, result)
    until (Pos('GROUP ( ', result) = 1) or Eof(txt);
    Close(txt)
  end { readSecondLine } ;


begin
  if FModuleHandle = NilHandle then
  begin
    FModuleHandle:= LoadLibrary(FModuleName);
    FLoadError := GetLoadErrorStr;
    FLastError := FLoadError;
    FAlreadyInMemory:= FModuleHandle <> NilHandle;
    IF NOT FAlreadyInMemory THEN BEGIN
      FModuleHandle := LoadLibrary(FModuleName);
      FLoadError := GetLoadErrorStr;
      FLastError := FLoadError;
      if FModuleHandle = NilHandle then
{$IFDEF UNIX }
        if (GetLastOSError = 0) and (Pos(': invalid ELF header', LastError) > 0) then begin

(* I believe this is a Debian special: libusb-0.1 has been declared obsolete    *)
(* and "replaced by a linker script, in the hope it will make everybody happy." *)
(* The error message above might be subject to i18n, so users in non-English    *)
(* speaking locales might have at least as much grief as I've just had. Blame   *)
(* Aurelien Jarno :-)                                                           *)

          revisedFilename := LastError;
          SetLength(revisedFilename, Pos(':', revisedFilename) - 1);
          revisedFilename := readSecondLine(revisedFilename);
          if Pos('GROUP ( ', revisedFilename) = 1 then begin
            Delete(revisedFilename, 1, Length('GROUP ( '));
            SetLength(revisedFilename, Pos(' )', revisedFilename) - 1);
            FModuleName := revisedFilename;
            LoadModule;                 (* Recursive                            *)

(* Exits on success, otherwise raises the same exception as below.              *)

            exit
          end
        end;
{$ENDIF }
        raise DynamicModuleException.Create(SysErrorMessage(GetLastOSError)
                                        + ' loading ' + FModuleName);
    END
  end
end { TDynamicModule.LoadModule } ;


(* Note that the name of the entry point is case-sensitive.
*)
procedure TDynamicModule.LoadRoutine(var Routine: Pointer; const Name: string);

begin
  if Routine = nil then
  begin
{$if defined(CPUX86_64) }
    Assert(PtrUInt(@Routine) mod 8 = 0, 'Internal error: processor-specific stack misalignment for GetProcedureAddress(2) A');
    Assert(PtrUInt(@Name) mod 8 = 0, 'Internal error: processor-specific stack misalignment for GetProcedureAddress(2) B');
{$endif                 }
    Routine := GetProcedureAddress(ModuleHandle, Name);
    FLastError := GetLoadErrorStr;
    if Routine = nil then
      raise DynamicModuleException.Create(FLastError)
  end
end { TDynamicModule.LoadRoutine } ;


(* This is an overloaded addition to Martin's original code. It can result in
  neater code but is much less efficient since it can't check whether it has
  already been called.
*)
function TDynamicModule.LoadRoutine(const Name: string): Pointer;

begin
{$if defined(CPUX86_64) }
  Assert(PtrUInt(@result) mod 8 = 0, 'Internal error: processor-specific stack misalignment for GetProcedureAddress(1) A');
  Assert(PtrUInt(@Name) mod 8 = 0, 'Internal error: processor-specific stack misalignment for GetProcedureAddress(1) B');
{$endif                 }
  RESULT:= GetProcedureAddress(ModuleHandle, Name);
  FLastError := GetLoadErrorStr;
  if RESULT = nil then
    raise DynamicModuleException.Create(FLastError)
end { TDynamicModule.LoadRoutine } ;


function TDynamicModule.ModuleExists(): Boolean;

begin
  try
    Result := ModuleHandle <> NilHandle
  except
    Result := False
  end
end { TDynamicModule.ModuleExists } ;


(* Note that the name of the entry point is case-sensitive.
*)
function TDynamicModule.RoutineExists(const Name: string): Boolean;

var
  Routine: Pointer;
begin
  Routine:= NIL;                        (* We're looking to see if this changes *)
  try
    LoadRoutine(Routine, Name);
  except
    Routine := nil;
  end;
  FLastError := GetLoadErrorStr;
  Result := Routine <> nil;
end { TDynamicModule.RoutineExists } ;


end.

