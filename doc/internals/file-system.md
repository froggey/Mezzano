<!--- -*- eval: (auto-fill-mode 1); eval: (flyspell-mode 1); -*- --->

# File Systems on Mezzano

Each type of file system on Mezzano requires a host class and two
stream classes.

One stream class must be a character stream; the other stream class
must be a binary stream. These streams must support the appropriate
stream methods.

One approach of creating these streams is to subclass the gray
streams:
  * gray:fundamental-character-input-stream and gray:fundamental-character-output-stream
  * gray:fundamental-binary-input-stream and gray:fundamental-binary-output-stream.

The host class must implement the following methods which are exported
by the mezzano.file-system package.

    parse-namestring-using-host (host namestring junk-allowed) => pathname

    namestring-using-host (host path) => namestring

    open-using-host (host path &key direction element-type if-exists if-does-not-exist external-format) => stream

    probe-using-host (host path) => pathname or NIL

    directory-using-host (host path &key) => list of pathnames

    ensure-directories-exist-using-host (host path &key verbose) => T or NIL

    rename-file-using-host (host source dest) => <ignored>

    file-write-date-using-host (host path) => Time at which file was last written

    file-author-using-host (host path) => Name of file owner

    delete-file-using-host (host path &key) => <ignored>

    expunge-directory-using-host (host path &key) => <ignored>

    truename-using-host (host path) => pathname

The function (setfable)

    mezzano.file-system:find-host (host-name &optional (errorp t)) => host

maps a host name to a host object. This mapping is used for converting
name strings to/from pathname objects.  This allows the syntax of the
name string to be host dependent. For example, the "REMOTE" host uses
Unix style name strings: "REMOTE:/home/tom/abc.lisp" while the "LOCAL"
host uses LispM style name strings: "LOCAL:>home>tom>abc.lisp".

For a file system that resides on a local disk or disk partition, the
host object must have a block device object so that read and write
requests can be mapped to the appropriate disk location. Removable
media, such as USB mass storage devices, also provide block device
objects.

When Mezzano boots, it enumerates all of the available disks and disk
partitions. A list of these objects can be obtained by calling:

    mezzano.supervisor:all-disks () => List of disks and partitions

These disks and partitions also support the block-device APIs:

    mezzano.disk:block-device-sector-size (disk) => <sector size in bytes>

    mezzano.disk:block-device-read (disk lba n-sectors buffer &key (offset 0)) => T on success, NIL and error otherwise

    mezzano.disk:block-device-write (disk lba n-sectors buffer &key (offset 0)) => T on success, NIL and error otherwise

    mezzano.disk:block-device-flush (disk) => T on success, NIL and error otherwise

A list of block devices can obtained by calling:

    mezzano.disk:all-block-devices () => List of block devices

All of the local disks and partitions enumerated on boot and returned
by mezzano.supervisor:all-disks are also returned by
mezzano.disk:all-block-devices. Additional block devices can be
registered by:

    mezzano.disk:register-block-device (device) => Updated list of block devices

And unregistered by:

    mezzano.disk:unregister-block-device (device) => Updated list of block devices

To create a new file system on a running system, create a host object
of the appropriate type with the desired host name and the desired
block device object. For example, the type could be fat32, the host
name could be "HOME", and the block device  selected from the results of
(mezzano.disk:all-block-devices). For this example, the host name/host
object mapping would be setup by

    (setf (mezzano.file-system:find-host  "HOME") <fat32 host object>)

If the system is rebooted without saving the current state using:

    mezzano.supervisor:snapshot () => <ignored>

then the host object will be lost and will need to be recreated and
mapped to the appropriate host name and associated with the
appropriate block device object.

However, if the system is saved, when the system is rebooted, the host
object and host name mapping will still exist and the host will be
re-associated with the block device via the following mechanism.

There is a class "file-host-mount-mixin" which includes a "mount-args"
slot which is accessed via an accessor function file-host-mount-args
and defines a generic function:

    mount (host) => T on success, NIL otherwise

and includes a default mount method which does nothing and returns T
(success).

All file system host classes include file-host-mount-mixin and when a
file system host is created, the mount-args slot is set to a value
that depends on both the host type and the file system associated with
the host. For example, for a FAT32 host, the mount-args slot would be
set to partition UUID, and for a NFS host, the mount-args slot might
be set to the url of the NFS file system, e.g.,
"nfs://nfs.lispm.net/export/home/tom".

During the boot process, after the disks and partitions are
enumerated, but before any file system references occur, mount is
called for each of the host objects registered with
mezzano.file-system:find-host. This method should:

  * delete the association of the host with the (old) block device;
  * call the appropriate function for "finding" that file system type
with the host type and the mount-args;
  * associate the host with the new block device object returned; and,
  * set the host field in the read/write object to the host (this
field does not currently exist in the disk object - see
[Future Work](#future-work) below).

Local block devices are supported by using the function:

    find-local-block-device (<class name> <partition UUID>) => <block device>

find-local-block-device will call the generic function

    probe-block-device (<class name> <block-device>) =>  <partition UUID> or NIL

on each block device object until a matching partition UUID is found
and find-local-block-device will return that block
device. probe-block-device will return NIL if the block device is not
formatted as the given host type. If no matching block device is
found, find-local-block-device will return NIL.

probe-block-device is a generic function that specializes on the class
name argument. Therefore, each file system host class that supports
block devices, will also need to define a probe-block-device
method. This method needs to read the header of the block device, make
enough checks to verify that the device contains a file system of the
appropriate type, then return the partition UUID (or equivalent).

This approach can be expanded to handle other kinds of file systems by
creating additional partition functions. For example, for NFS the
following function might be defined:

    find-nfs-partition (<nfs url>) => <nfs read/write object>

For the currently existing http host, remote host, local host and sys
host, the mount method does nothing except return success. This is all
that is required as these hosts do not need to do anything special on
reboot.

The fat host defines a mount method and an appropriate
probe-block-device method. And therefore, any fat host saved by a
snapshot will be available on reboot.

Currently (6 April 2020) the ext4 host mount method returns NIL and no
probe-block-device method is provided. This means any ext4 host saved
by a snapshot will **not** be available on reboot.

## Future Work

Adding reboot support for ext4.

Adding the host field to the block device object (or other read/write
object) makes it easier for an application, e.g., the name space
editor, to list block devices and their associated host objects. In
addition, the change to probe-block-device to return the partition
UUID instead of doing the match allows probe-block-device to be used
to determine the file system type of a block device without knowing
the partition UUID. For example, when a disk or partition is formatted
but not yet associated with a host object.

Create new generic function API in mezzano.file-system package:

    mount-file-system (<class name> <host name> <block device>) => host

which specializes on the class name argument, for example,
mount-file-system('fat-host "HOME" <block device>). The function
verifies that the block device contains a file system of the correct
type, creates a host object and registers it using (setf
(mezzano.file-system:find-host host-name) <host object>). This
function would be useful in general, but also by the name space
editor. Currently (6 April 2020), for FAT, this functionality is
provided by mezzano.fat-file-system::mount-fat(block-device host-name
uuid).
