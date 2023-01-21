(* Lazarus v1.0.0 FPC 2.6.0 i386-Linux Lazarus v1.0.0 FPC 2.6.0 i386-Linux FPC  *)

unit LibCapDynamic;

(* This is a very much cut-down interface to the libcap library. In practical   *)
(* terms this is limited to removing capabilities which have been granted to an *)
(* executable by the setcap utility, or adding effective capabilities where a   *)
(* permitted capability already exists.                                         *)
(*                                                                              *)
(* Note that Debian ships libcap as a .so rather than a .a, and Solaris 10 has  *)
(* neither (although Mozilla contributes an unrelated libcaps.so). Hence in     *)
(* practice it's probably most use to link this dynamically, and to try to "do  *)
(* the right thing" if the library's unavailable.               MarkMLl.        *)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DynamicModule;

type    cap_t= Pointer;

(* These types are an accurate description of the entry points exported by the  *)
(* static or shared library.                                                    *)

type    Tcap_get_proc= function: cap_t; cdecl;
        Tcap_set_proc= function(cap_p: cap_t): longint; cdecl;
        Tcap_free= function(cap_p: cap_t): longint; cdecl;
        Tcap_get_flag= function(cap_p: cap_t; cap: longint; flag: longint;
                        var value: longint): longint; cdecl;
        Tcap_set_flag= function(cap_p: cap_t; flag: longint; ncaps :longint;
                        caps: Pointer; setting: longint): longint; cdecl;

        TLibCapShim= class(TDynamicModule)
                     private
                       fcap_get_proc: Tcap_get_proc;
                       fcap_set_proc: Tcap_set_proc;
                       fcap_free: Tcap_free;
                       fcap_get_flag: Tcap_get_flag;
                       fcap_set_flag: Tcap_set_flag;
                     public
                       function cap_get_proc: cap_t; cdecl;
                       function cap_set_proc(cap_p: cap_t): longint; cdecl;
                       function cap_free(cap_p: cap_t): longint; cdecl;
                       function cap_get_flag(cap_p: cap_t; cap: longint; flag: longint;
                                        var value: longint): longint; cdecl;
                       function cap_set_flag(cap_p: cap_t; flag: longint; ncaps :longint;
                                        caps: Pointer; setting: longint): longint; cdecl;
                     end;

var     LoadErrorStr: string;

function LibCapShim: TLibCapShim;


implementation

var     xLibCapShim: TLibCapShim= nil;


function LibCapShim: TLibCapShim;

begin
  result := xLibCapShim
end { LibCapShim } ;


function TLibCapShim.cap_get_proc: cap_t; cdecl;

begin
  LoadRoutine(pointer(fcap_get_proc), 'cap_get_proc');
  result := fcap_get_proc()
end { TLibCapShim.cap_get_proc } ;


function TLibCapShim.cap_set_proc(cap_p: cap_t): longint; cdecl;

begin
  LoadRoutine(pointer(fcap_set_proc), 'cap_set_proc');
  result := fcap_set_proc(cap_p)
end { TLibCapShim.cap_set_proc } ;


function TLibCapShim.cap_free(cap_p: cap_t): longint; cdecl;

begin
  LoadRoutine(pointer(fcap_free), 'cap_free');
  result := fcap_free(cap_p)
end { TLibCapShim.cap_free } ;


function TLibCapShim.cap_get_flag(cap_p: cap_t; cap: longint; flag: longint;
                 var value: longint): longint; cdecl;

begin
  LoadRoutine(pointer(fcap_get_flag), 'cap_get_flag');
  result := fcap_get_flag(cap_p, cap, flag, value)
end { TLibCapShim.cap_get_flag } ;


function TLibCapShim.cap_set_flag(cap_p: cap_t; flag: longint; ncaps :longint;
                 caps: Pointer; setting: longint): longint; cdecl;

begin
  LoadRoutine(pointer(fcap_set_flag), 'cap_set_flag');
  result := fcap_set_flag(cap_p, flag, ncaps, caps, setting)
end { TLibCapShim.cap_set_flag } ;


initialization
  try
    xLibCapShim := TLibCapShim.Create('libcap.so');
    LoadErrorStr := xLibCapShim.LastError;
    xLibCapShim.LoadModule;
    LoadErrorStr := xLibCapShim.LastError
  except
    xLibCapShim := nil
  end
finalization
  if Assigned(LibCapShim) then
    xLibCapShim.Free
end.

