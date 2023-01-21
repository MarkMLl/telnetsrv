# telnetsrv
This provides moderately-competent Telnet server and client objects, adequate to replace the LNet implementation. Operation may be threaded or polled.

Both server and client include hooks which allow telnet option handlers to be defined, including support for the standard DO/DONT/WILL/WONT options and suboptions such as the terminal type. This has been tested with- as a specific example- a client program connecting to the Hercules terminal emulator.

The server has a non-standard PIN facility, which allows it to be used for e.g. presentation control without some kiddie in the audience breaking in.

The server is also aware of POSIX capabilities, so may potentially be used with e.g. the standard Telnet port: privileges are surrendered before the socket is exposed.

An application program acting as a server may intercept a program's INPUT and OUTPUT text devices. This was originally intended as a debugging interface, but may also be used for e.g. planetarium control.

A client program is presented with an API resembling the standard serial.pp interface, although additional facilities are available.
