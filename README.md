# telnetsrv
This provides moderately-competent Telnet server and client objects, adequate to replace the LNet implementation and somewhat better behaved when handling e.g. rapid client reconnection. Operation may be threaded or (with limitations) polled.

Both server and client include hooks which allow telnet option handlers to be defined, including support for the standard DO/DONT/WILL/WONT options, suboptions such as the terminal type, and the standard NVT controls. This has been tested with- as a specific example- a client program connecting to the Hercules terminal emulator, this didn't work with unmodified LNet.

The server has a non-standard PIN facility, which allows it to be used for e.g. presentation control without some kiddie in the audience breaking in. Don't expect this to work without a background thread, and in any event expect it to be messed up by any client that tries to negotiate options prematurely.

The server is also aware of POSIX capabilities, so may potentially be used with e.g. the standard Telnet port: privileges are surrendered before the socket is exposed.

An application program acting as a server may intercept a program's INPUT and OUTPUT text devices. This was originally intended as a debugging interface, but may also be used for e.g. planetarium control. See telnetdemo.lpr etc. for an example.

A client program is presented with an API resembling the standard serial.pp interface, although additional facilities are available. See telnettest.lpr etc. for an example.

KNOWN ISSUE: if a telnet server is destroyed the INPUT and OUTPUT devices are restored, but this is minimally tested.
