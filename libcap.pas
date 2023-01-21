(* Lazarus v1.0.0 FPC 2.6.0 i386-Linux Lazarus v1.0.0 FPC 2.6.0 i386-Linux FPC  *)

unit LibCap;

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

const   CAP_EFFECTIVE= 0;               (* Specifies the effective flag         *)
        CAP_PERMITTED= 1;               (* Specifies the permitted flag         *)
        CAP_INHERITABLE= 2;             (* Specifies the inheritable flag       *)

        CAP_CLEAR= 0;                   (* The flag is cleared/disabled         *)
        CAP_SET= 1;                     (* The flag is set/enabled              *)

(* ** POSIX-draft defined capabilities.                                         *)

{ In a system with the [_POSIX_CHOWN_RESTRICTED] option defined, this
   overrides the restriction of changing file ownership and group
   ownership.  }
//
        CAP_CHOWN = 0;

{ Override all DAC access, including ACL execute access if
   [_POSIX_ACL] is defined. Excluding DAC access covered by
   CAP_LINUX_IMMUTABLE.  }
//
        CAP_DAC_OVERRIDE = 1;

{ Overrides all DAC restrictions regarding read and search on files
   and directories, including ACL restrictions if [_POSIX_ACL] is
   defined. Excluding DAC access covered by CAP_LINUX_IMMUTABLE.  }
//
        CAP_DAC_READ_SEARCH = 2;

{ Overrides all restrictions about allowed operations on files, where
   file owner ID must be equal to the user ID, except where CAP_FSETID
   is applicable. It doesn't override MAC and DAC restrictions.  }
//
        CAP_FOWNER = 3;

{ Overrides the following restrictions that the effective user ID
   shall match the file owner ID when setting the S_ISUID and S_ISGID
   bits on that file; that the effective group ID (or one of the
   supplementary group IDs) shall match the file owner ID when setting
   the S_ISGID bit on that file; that the S_ISUID and S_ISGID bits are
   cleared on successful return from chown(2) (not implemented).  }
//
        CAP_FSETID = 4;

{ Used to decide between falling back on the old suser() or fsuser().  }
//
        CAP_FS_MASK = $1f;

{ Overrides the restriction that the real or effective user ID of a
   process sending a signal must match the real or effective user ID
   of the process receiving the signal.  }
//
        CAP_KILL = 5;

{ Allows setgid(2) manipulation  }
{ Allows setgroups(2)  }
{ Allows forged gids on socket credentials passing.  }
//
        CAP_SETGID = 6;

{ Allows set*uid(2) manipulation (including fsuid).  }
{ Allows forged pids on socket credentials passing.  }
//
        CAP_SETUID = 7;

(* ** Linux-specific capabilities.                                              *)

{ Transfer any capability in your permitted set to any pid,
   remove any capability in your permitted set from any pid  }
//
        CAP_SETPCAP = 8;

{ Allow modification of S_IMMUTABLE and S_APPEND file attributes  }
//
        CAP_LINUX_IMMUTABLE = 9;

{ Allows binding to TCP/UDP sockets below 1024  }
{ Allows binding to ATM VCIs below 32  }
//
        CAP_NET_BIND_SERVICE = 10;

{ Allow broadcasting, listen to multicast  }
//
        CAP_NET_BROADCAST = 11;

{ Allow interface configuration  }
{ Allow administration of IP firewall, masquerading and accounting  }
{ Allow setting debug option on sockets  }
{ Allow modification of routing tables  }
{ Allow setting arbitrary process / process group ownership on
   sockets  }
{ Allow binding to any address for transparent proxying  }
{ Allow setting TOS (type of service)  }
{ Allow setting promiscuous mode  }
{ Allow clearing driver statistics  }
{ Allow multicasting  }
{ Allow read/write of device-specific registers  }
{ Allow activation of ATM control sockets  }
//
        CAP_NET_ADMIN = 12;

{ Allow use of RAW sockets  }
{ Allow use of PACKET sockets  }
//
        CAP_NET_RAW = 13;

{ Allow locking of shared memory segments  }
{ Allow mlock and mlockall (which doesn't really have anything to do
   with IPC)  }
//
        CAP_IPC_LOCK = 14;

{ Override IPC ownership checks  }
//
        CAP_IPC_OWNER = 15;

{ Insert and remove kernel modules - modify kernel without limit  }
{ Modify cap_bset  }
//
        CAP_SYS_MODULE = 16;

{ Allow ioperm/iopl access  }
{ Allow sending USB messages to any device via /proc/bus/usb  }
//
        CAP_SYS_RAWIO = 17;

{ Allow use of chroot()  }
//
        CAP_SYS_CHROOT = 18;

{ Allow ptrace() of any process  }
//
        CAP_SYS_PTRACE = 19;

{ Allow configuration of process accounting  }
//
        CAP_SYS_PACCT = 20;

{ Allow configuration of the secure attention key  }
{ Allow administration of the random device  }
{ Allow examination and configuration of disk quotas  }
{ Allow configuring the kernel's syslog (printk behaviour)  }
{ Allow setting the domainname  }
{ Allow setting the hostname  }
{ Allow calling bdflush()  }
{ Allow mount() and umount(), setting up new smb connection  }
{ Allow some autofs root ioctls  }
{ Allow nfsservctl  }
{ Allow VM86_REQUEST_IRQ  }
{ Allow to read/write pci config on alpha  }
{ Allow irix_prctl on mips (setstacksize)  }
{ Allow flushing all cache on m68k (sys_cacheflush)  }
{ Allow removing semaphores  }
{ Used instead of CAP_CHOWN to "chown" IPC message queues, semaphores
   and shared memory  }
{ Allow locking/unlocking of shared memory segment  }
{ Allow turning swap on/off  }
{ Allow forged pids on socket credentials passing  }
{ Allow setting readahead and flushing buffers on block devices  }
{ Allow setting geometry in floppy driver  }
{ Allow turning DMA on/off in xd driver  }
{ Allow administration of md devices (mostly the above, but some
   extra ioctls)  }
{ Allow tuning the ide driver  }
{ Allow access to the nvram device  }
{ Allow administration of apm_bios, serial and bttv (TV) device  }
{ Allow manufacturer commands in isdn CAPI support driver  }
{ Allow reading non-standardized portions of pci configuration space  }
{ Allow DDI debug ioctl on sbpcd driver  }
{ Allow setting up serial ports  }
{ Allow sending raw qic-117 commands  }
{ Allow enabling/disabling tagged queuing on SCSI controllers and sending
  arbitrary SCSI commands  }
{ Allow setting encryption key on loopback filesystem  }
//
        CAP_SYS_ADMIN = 21;

{ Allow use of reboot()  }
//
        CAP_SYS_BOOT = 22;

{ Allow raising priority and setting priority on other (different
  UID) processes  }
{ Allow use of FIFO and round-robin (realtime) scheduling on own
  processes and setting the scheduling algorithm used by another
  process.  }
//
        CAP_SYS_NICE = 23;

{ Override resource limits. Set resource limits.  }
{ Override quota limits.  }
{ Override reserved space on ext2 filesystem  }
{ NOTE: ext2 honors fsuid when checking for resource overrides, so
  you can override using fsuid too  }
{ Override size restrictions on IPC message queues  }
{ Allow more than 64hz interrupts from the real-time clock  }
{ Override max number of consoles on console allocation  }
{ Override max number of keymaps  }
//
        CAP_SYS_RESOURCE = 24;

{ Allow manipulation of system clock  }
{ Allow irix_stime on mips  }
{ Allow setting the real-time clock  }
//
        CAP_SYS_TIME = 25;

{ Allow configuration of tty devices  }
{ Allow vhangup() of tty  }
//
        CAP_SYS_TTY_CONFIG = 26;

{ Allow the privileged aspects of mknod()  }
//
        CAP_MKNOD = 27;

{ Allow taking of leases on files  }
//
        CAP_LEASE = 28;

(* Set or reset a capability.                                                   *)
//
function SetCapability(value: boolean; capability: longint; flag: longint= CAP_EFFECTIVE): boolean;

(* Get the state of the capability, the value is set true on error since        *)
(* thinking that a capability is unavailable (when it is really available and   *)
(* could be used by a hostile intruder) is a more serious flaw than thinking it *)
(* is available (when the fact that it isn't will be demonstrated by a          *)
(* subsequent error).                                                           *)
//
function GetCapability(var value: boolean; capability: longint; flag: longint= CAP_EFFECTIVE): boolean;

type    cap_t= Pointer;

function cap_get_proc: cap_t; cdecl;
function cap_set_proc(cap_p: cap_t): longint; cdecl;
function cap_free(cap_p: cap_t): longint; cdecl;
function cap_get_flag(cap_p: cap_t; cap: longint; flag: longint;
                        var value: longint): longint; cdecl;
function cap_set_flag(cap_p: cap_t; flag: longint; ncaps :longint;
                        caps: Pointer; setting: longint): longint; cdecl;


implementation

uses {$ifdef USE_DYNAMIC_LIBCAP } LibCapDynamic {$else } LibCapShim {$endif } ;


(* Set or reset a capability.                                                   *)
//
function SetCapability(value: boolean; capability: longint; flag: longint= CAP_EFFECTIVE): boolean;

var     caps: cap_t;
        caplist, r: longint;
        {%H- 5027 } s: string;          (* For debugging dynamic linkage        *)

begin
  result := false;
{$ifdef USE_DYNAMIC_LIBCAP }
  if LibCapShim = nil then              (* Initialisation failed                *)
    exit;
{$endif                    }
  caps := LibCapShim.cap_get_proc;
  s := LibCapShim.LastError;
  if caps <> nil then
    try
      caplist := capability;
      r := LibCapShim.cap_set_flag(caps, flag, 1, @caplist, Ord(value));
      s := LibCapShim.LastError;
      if r = 0 then begin               (* Leave r visible for debugging        *)
        result := LibCapShim.cap_set_proc(caps) = 0;
        s := LibCapShim.LastError
      end
    finally
      LibCapShim.cap_free(caps);
      s := LibCapShim.LastError
    end
end { SetCapability } ;


(* Get the state of the capability, the value is set true on error since        *)
(* thinking that a capability is unavailable (when it is really available and   *)
(* could be used by a hostile intruder) is a more serious flaw than thinking it *)
(* is available (when the fact that it isn't will be demonstrated by a          *)
(* subsequent error).                                                           *)
//
function GetCapability(var value: boolean; capability: longint; flag: longint= CAP_EFFECTIVE): boolean;

var     caps: cap_t;
        v, r: longint;
        {%H- 5027 } s: string;          (* For debugging dynamic linkage        *)

begin
  result := false;
  value := true;
{$ifdef USE_DYNAMIC_LIBCAP }
  if LibCapShim = nil then              (* Initialisation failed                *)
    exit;
{$endif                    }
  caps := LibCapShim.cap_get_proc;
  s := LibCapShim.LastError;
  if caps <> nil then
    try

(* The documented result of this function is zero for success, but this might   *)
(* not be reliable and appears to be ignored by most example code. The returned *)
(* value is documented as 0 or 1, but I've seen other values in response to bad *)
(* parameters. Possibly disregard the function result, but always look          *)
(* carefully at the value.                                                      *)

      r := LibCapShim.cap_get_flag(caps, capability, flag, v);
      s := LibCapShim.LastError;
      result := r = 0;                  (* Leave r visible for debugging        *)
      if result then
        case v of
          0, 1: value := v <> 0
        else
          result := false
        end
    finally
      LibCapShim.cap_free(caps);
      s := LibCapShim.LastError
    end
end { GetCapability } ;


function cap_get_proc: cap_t; cdecl;

begin
  result := LibCapShim.cap_get_proc
end { cap_get_proc } ;


function cap_set_proc(cap_p: cap_t): longint; cdecl;

begin
  result := LibCapShim.cap_set_proc(cap_p)
end { cap_set_proc } ;


function cap_free(cap_p: cap_t): longint; cdecl;

begin
  result := LibCapShim.cap_free(cap_p)
end { cap_free } ;


function cap_get_flag(cap_p: cap_t; cap: longint; flag: longint;
                        var value: longint): longint; cdecl;

begin
  result := LibCapShim.cap_get_flag(cap_p, cap, flag, value)
end { cap_get_flag } ;


function cap_set_flag(cap_p: cap_t; flag: longint; ncaps :longint;
                        caps: Pointer; setting: longint): longint; cdecl;

begin
  result := LibCapShim.cap_set_flag(cap_p, flag, ncaps, caps, setting)
end { cap_set_flag } ;


end.

