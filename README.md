#Mezzano, an operating system written in Common Lisp.

`C-<key>` means to hold the control key while typing `<key>`.

`M-<key>` means to hold the alt or meta key while typing `<key>`.

Alphabetic keys typed with control or meta ignore case. `C-A` and `C-a` are the same key, while `C-!` and `C-1` are different.

The default keymap is En-GB, use M-F12 to switch between En-GB, En-US, No-BK, PanCyr, German, and ES-ESP keymaps.
Windows can be moved by dragging their titlebar or by holding the Alt key and dragging.

For help & support, join #mezzano on Freenode (irc.freenode.net)

`M-Esc` will attempt to interrupt the thread associated with the current window.
This won't work if the thread is stuck in a tight loop or if the thread is blocked.

#Line editing
===
The line editor supports most standard line navigation and editing commands.

`C-F`          Move forward (right) one character, also bound to Right-Arrow.

`C-B`          Move backward (left) one character, also bound to Left-Arrow.

`C-A`          Move to beginning of line, also bound to Home.

`C-E`          Move to end of line, also bound to End.

`M-F`          Move forward one word.

`M-B`          Move backward one word.

`M-P`          Find previous (older) matching history item, also bound to Up-Arrow.

`M-N`          Find next (newer) matching history item, also bound to Down-Arrow.

`C-D`          Delete the next character, also bound to Delete.

`Backspace`    Delete the previous character.

`M-D`          Delete the next word.

`M-Backspace`  Delete the previous word.

`C-K`          Delete from the cursor to the end of the line.

`C-C`          Enter the debugger using BREAK.

`C-G`          Invoke the most recent ABORT restart. This will usually clear any input and return you to a prompt.

`Tab`          Cycle through completions for the current symbol.

#Editor commands
===
The editor mostly follows Emacs conventions.

`C-F`          Move forward (right) one character, also bound to Right-Arrow.

`C-B`          Move backward (left) one character, also bound to Left-Arrow.

`C-N`          Move to the next line (down), also bound to Down-Arrow.

`C-P`          Move to the previous line (up), also bound to Up-Arrow.

`C-A`          Move to beginning of line.

`C-E`          Move to end of line.

`M-<`          Move to the beginning of the buffer, also bound to Home.

`M->`          Move to the end of the buffer, also bound to End.

`C-V`          Move the point to the bottom of the screen and recenter, also bound to Page-Down.

`M-V`          Move the point to the top of the screen and recenter, also

bound to Page-Up.

`M-F`          Move forward one word.

`M-B`          Move backward one word.

`C-M-F`        Move forward one s-expression.

`C-M-F`        Move backward one s-expression.

`C-D`          Delete the next character, also bound to Delete.

`Backspace`    Delete the previous character.

`M-D`          Kill the next word.

`M-Backspace`  Kill the previous word.

`C-K`          Kill characters from the point to the end of the line, or kill

the newline if the point is at the end of the line.

`C-M-K`        Kill the next s-expression forward.

`C-W`          Kill the area between the point and the mark.

`C-Y`          Yank the last killed text back into the buffer at the point.

`C-L`          Recenter the display on the point.

`M-L`          Redraw the screen.

`C-Q`          Insert the next key typed without intepretting it as a command.

`C-Space`      If the point is at the mark and the mark is active, deactivate the mark. Otherwise, activate the mark and move it to the point.

`C-X C-X`      Swap the point and the mark.

`C-X C-F`      Open or create a file.

`C-X C-S`      Save the current buffer. If the buffer has no path, you will be prompted for a location to save it.

`C-X C-W`      Save the current buffer with a new path.

`C-X b`        Switch to a different buffer.

`C-X C-B`      List buffers.

`C-X k`        Close an open buffer.

`C-G`          Abort the current command.

`C-C C-C`      Evaluate the current top-level form.

`C-C C-A`      Move to the start of the current top-level form.

`M-x repl`     Create an editor-based REPL.

#Swank

This release includes Swank 2016-03-04.
The Swank server is listening on port 4005.

Forwarding for port 4005 will need to be enabled in the virtual
machine's settings. In VirtualBox this is done throught the port
forwarding settings, which can be accessed through
Settings -> Network -> Advanced -> Port Forwarding

#Major changes since Demo 2
Trentino, a media player, has been implemented by Eugene Zaikonnikov.
Further improvements to conformance, stability and performance.
The CLOS implementation follows the MOP much more closely.
More traditional window management.
Booting from CD/USB on real hardware is now possible.
Driver support for Intel HDA audio devices.
VirtualBox guest (mouse & display) integration.

#Major changes since Demo 1

Many improvements to conformance, stability and performance.
The editor has been greatly improved, thanks to Burton Samograd.
The system now functions correctly on computers with more than 1GB of RAM.
The allocator and garbage collector now make much better use of available
memory, with far fewer GC cycles occuring.
(ROOM T) prints more detailed information about allocated objects.
Transparency and premultiplied alpha support in the GUI.
And more!

#Blinkenlights

A number of status lights are displayed at the top left of the screen.
From left to right:
`Green`        Disk read in progress.

`Red`          Disk write in progress.

`Purple`       GC in progress.

`Cyan`         Activity, system is not idle.

`Yellow`       Snapshot in progress.

`Brown`        Page fault being serviced.

`Light Green`  Network activity.

* The entire top line will turn red if the system panics.

#Memory Monitor

The memory monitor displays a bitmap indicating how each page of physical memory
is used. Colours indicate type.
`Blue`         Free memory.

`Red`          Wired memory.

`Brown`        Wired backing memory, used during a snapshot.

`Green`        Active in-use memory.

`Dark green`   Active in-use memory ready to be written to disk.

`Purple`       Inactive memory ready to be written to disk.

`Pink`         Page tables.

`Grey`         Other.

`White`        Mixture.

`Black`        Unused or not present.

#Installation
See the MBuild repo for installation: (https://github.com/froggey/MBuild)

#Included Libraries

ASDF 3, Alexandria, Babel, Bordeaux-threads, Chipz, cl-jpeg, cl-riff,
cl-vectors, flexi-streams, iterate, png-read, skippy, slime,
trivial-features, trivial-gray-streams, and zpb-ttf.

Full source code is available under LOCAL:>Source>

"Hypothymis azurea - Kaeng Krachan" by JJ Harrison (jjharrison89@facebook.com)
[CC BY-SA 3.0 (http://creativecommons.org/licenses/by-sa/3.0)], via Wikimedia Commons
https://commons.wikimedia.org/wiki/File:Hypothymis_azurea_-_Kaeng_Krachan.jpg

"Mandarin Pair" by © Francis C. Franklin / CC-BY-SA-3.0.
Licensed under CC BY-SA 3.0 via Wikimedia Commons - http://commons.wikimedia.org/wiki/File:Mandarin_Pair.jpg

Includes Dejavu Fonts 2.35 (http://dejavu-fonts.org/)

Some icons from Icojam (http://www.icojam.com)


#Whole-system transparent persistence support

Run `(mezzano.supervisor:snapshot)` in a REPL.
Wait for the yellow light to turn off.
Reboot.

This will take a snapshot of the current machine state, saving it to disk.
It will be restored when the machine is booted.
