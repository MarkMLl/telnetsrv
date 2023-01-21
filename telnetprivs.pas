(* Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+FPC 0.9.24+2.2.4 on Linux Lazarus+ *)
(* Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FPC 2.2.4+3.2.2 on Linux Lazarus+FP *)

unit TelnetPrivs;

(* Various things relating to POSIX Capabilities etc., slightly cut-down from   *)
(* WatchP0x.                                                    MarkMLl.        *)

(* Note that I haven't a clue whether selection of appropriate capabilities     *)
(* and/or setuid etc. flags would allow login to be run, or equivalent access   *)
(* via PAM to the system's password authentication in order to allow controlled *)
(* execution of a general-purpose shell. This is "left as an exercise", but     *)
(* will be needed before any serious implementation of Kermit etc.              *)

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

// TODO : Always be verbose if any capability existed or execution was setuid etc.
{ define VERBOSE }

(* Relinquish the capabilities which allowed a non-root user to create a socket
  with a port number < 1024.
*)
procedure RelinquishCapabilities;

(* Relinquish privileged uid and gid.
*)
procedure RelinquishRootIdentity;

(* Check our privilege.
*)
procedure ReportPrivilege(const msg: AnsiString);


implementation

uses
  BaseUnix, LibCap;

(* If libcap is to be linked statically it might require installation of the    *)
(* libcap-dev package on Debian in order for static linkage to succeed.         *)


(* Relinquish the capabilities which allowed a non-root user to create a socket
  with a port number < 1024.
*)
procedure RelinquishCapabilities;

var
  cap: boolean;

begin

(* When not running as root, relinquish any capabilities we've been granted.    *)
(* Even if running as root do this silently, to allow for the case where a      *)
(* capability has been explicitly added. Note that I'm avoiding "permissions"   *)
(* etc. here as ambiguous, I don't think there's any need to translate          *)
(* "capability" etc. in this context.                                           *)

(* WARNING: visibility of capabilities might be modified if running under the   *)
(* control of the debugger. Always test outside the debugger before jumping to  *)
(* any conclusions.                                                             *)

  if (FpGetgid <> 0) or (FpGetuid <> 0) then begin      (* Give up capability   *)
    if GetCapability(cap, CAP_NET_BIND_SERVICE) then
      if cap then begin
{$ifdef VERBOSE }
        Write(StdErr, '# Relinquishing NET_BIND_SERVICE capability... ');
{$endif VERBOSE }
        if not SetCapability(false, CAP_NET_BIND_SERVICE) then
{$ifdef VERBOSE }
          WriteLn(StdErr, 'failed')
        else
          WriteLn(StdErr, 'OK')
{$else          }
        WriteLn(StdErr, '# Relinquishing NET_BIND_SERVICE capability... failed')
{$endif VERBOSE }
      end else begin end;
    if GetCapability(cap, CAP_NET_BIND_SERVICE, CAP_PERMITTED) then
      if cap then begin
{$ifdef VERBOSE }
        Write(StdErr, '# Relinquishing NET_BIND_SERVICE permittivity... ');
{$endif VERBOSE }
        if not SetCapability(false, CAP_NET_BIND_SERVICE, CAP_PERMITTED) then
{$ifdef VERBOSE }
          WriteLn(StdErr, 'failed')
        else
          WriteLn(StdErr, 'OK')
{$else          }
        WriteLn(StdErr, '# Relinquishing NET_BIND_SERVICE permittivity... failed')
{$endif VERBOSE }
      end else begin end
  end else begin
    SetCapability(false, CAP_NET_BIND_SERVICE);
    SetCapability(false, CAP_NET_BIND_SERVICE, CAP_PERMITTED)
  end
end { RelinquishCapabilities } ;


(* Get the uid and gid for the indicated file. In case of error return 65534
  which is nobody/nogroup.
*)
procedure getOwner(const filename: AnsiString; var uid: TUid; var gid: TGid);

var
  statBuff: Stat;                       (* Architecture-specific                *)

begin
  uid := 65534;
  gid := 65534;
  try
    if fpStat(filename, statbuff) = 0 then begin
{$if FPC_FULLVERSION < 030200 }          (* Requires FPC 2.2.4 minimum          *)
      uid := statBuff.uid;
      gid := statBuff.gid
{$else                         }
      uid := statBuff.st_uid;
      gid := statBuff.st_gid
{$endif FPC_FULLVERSION        }
    end
  except
  end
end { getOwner } ;


(* Relinquish privileged uid and gid.
*)
procedure RelinquishRootIdentity;

var
  uid: TUid;
  gid: TGid;

begin
  getOwner(ParamStr(0), uid, gid);

(* If running setuid/setgid then revert to the actual user/group, if running as *)
(* root switch to the user/group from the .ini file or owning the executable.   *)

// TODO : Fix EUID and EGID if running setuid.

  if FpGetgid = 0 then                  (* Must relinquish root                 *)
    if FpGetegid <> 0 then              (* Revert to original GID               *)
      FpSetgid(FpGetegid)
    else                                (* Use GID from .ini or executable      *)
      FpSetgid(gid);
  if FpGetuid = 0 then                  (* Must relinquish root                 *)
    if FpGeteuid <> 0 then              (* Revert to original UID               *)
      FpSetuid(FpGeteuid)
    else                                (* Use UID from .ini or executable      *)
      FpSetuid(uid)
end { RelinquishRootIdentity } ;


(* Check our privilege.
*)
procedure ReportPrivilege(const msg: AnsiString);

var
  capC, capP: boolean;


  function bLower(b: boolean): AnsiString;

  begin
    if b then
      result := 'true'
    else
      result := 'false'
  end { bLower } ;


begin
  capC := false;
  capP := false;
  try // TODO : Needs a library version check here (test on Debian "Lenny").
    if not (GetCapability(capC, CAP_NET_BIND_SERVICE) and
                          GetCapability(capP, CAP_NET_BIND_SERVICE, CAP_PERMITTED)) then
      WriteLn(StdErr, '# Unable to check CAP_NET_BIND_SERVICE')
{$ifdef VERBOSE }
    else
      WriteLn(StdErr, '# ', msg + ' CAP_NET_BIND_SERVICE ', bLower(capC or capP))
{$endif VERBOSE }
  except
    WriteLn(StdErr, '# Exception while checking CAP_NET_BIND_SERVICE')
  end;
{$ifdef VERBOSE }
  WriteLn(StdErr, '# Running as UID ', FpGetuid, ' EUID ', FpGeteuid, ' GID ', FpGetgid, ' EGID ', FpGetegid)
{$endif VERBOSE }
end { ReportPrivilege } ;


end.

