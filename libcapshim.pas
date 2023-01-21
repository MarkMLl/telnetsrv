(* Lazarus v1.0.0 FPC 2.6.0 i386-Linux Lazarus v1.0.0 FPC 2.6.0 i386-Linux FPC  *)

unit LibCapShim;

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
  Classes, SysUtils;

(* This was generated from capability.h with the assistance of h2pas -d -l cap  *)

type    cap_t= Pointer;

(* These functions are an accurate description of the entry points exported by  *)
(* the static or shared library.                                                *)

function cap_get_proc: cap_t; cdecl; external;
function cap_set_proc(cap_p: cap_t): longint; cdecl; external;
function cap_free(cap_p: cap_t): longint; cdecl; external;
function cap_get_flag(cap_p: cap_t; cap: longint; flag: longint;
                        var value: longint): longint; cdecl; external;
function cap_set_flag(cap_p: cap_t; flag: longint; ncaps :longint;
                        caps: Pointer; setting: longint): longint; cdecl; external;

const   LastError= '';                  (* Not meaningful for static linkage    *)

implementation

{$linklib libcap }

end.

