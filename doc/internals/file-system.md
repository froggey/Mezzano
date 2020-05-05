<!--- -*- eval: (auto-fill-mode 1); eval: (flyspell-mode 1); -*- --->

# File Systems on Mezzano

Each type of file system on Mezzano requires a host class which must
be a superclass of file-system-host and implement the following
methods:

    parse-namestring-using-host (host namestring junk-allowed) => pathname

    namestring-using-host (path host) => namestring

    open-using-host (host pathname &key direction element-type if-exists if-does-not-exist external-format) => stream

    probe-using-host (host pathname) => pathname or NIL

    directory-using-host (host path &key) => list of pathnames

    ensure-directories-exist-using-host (host pathname &key verbose) => T or NIL

    rename-file-using-host (host source dest) => <ignored>

    file-write-date-using-host (host path) => Time at which file was last written

    file-author-using-host (host path) => Name of file owner

    delete-file-using-host (host path &key) => <ignored>

    expunge-directory-using-host (host path &key) => <ignored>

    delete-directory-using-host (host path &key) =>

    truename-using-host (host pathname) => pathname

These methods and file-system-host are exported by the
mezzano.file-system package.

The default implementation for truename-using-host calls probe-file
(which calls probe-file-using-host). If this implementation is
sufficient for a given host, then no host specific implementation of
truename-using-host is required.

As listed above, open-using-host returns a stream. Usually, the
stream is one of two types: a character stream or a binary
stream. These streams may be implemented by subclassing the gray streams:

  * gray:fundamental-character-input-stream and gray:fundamental-character-output-stream
  * gray:fundamental-binary-input-stream and gray:fundamental-binary-output-stream.

The function (setfable)

    mezzano.file-system:find-host (host-name &optional (errorp t)) => host

maps a host name to a host object. This mapping is used for converting
namestrings to/from pathname objects.  This allows the syntax of the
namestring to be host dependent. For example, the "REMOTE" host uses
Unix style namestrings: "REMOTE:/home/tom/abc.lisp" while the "LOCAL"
host uses LispM style namestrings: "LOCAL:>home>tom>abc.lisp".

File systems that use a local disk or disk partition, either fixed or
removable, must also be a superclass of file-host-mount-mixin and
implement the following methods:

    create-host (class block-device name-alist) => name on success, otherwise NIL

    mount-host (host block-device) => T on success, otherwise NIL

The create-host method specializes on the class argument; the
mount-host method specializes on the host argument; and, the
name-alist is a list of the form:

    ((UUID <host name>)...)

The pairs are not dotted pairs and the host name should be a
string. File system host classes that are superclasses of
file-host-mount-mixin must register using the following:

    register-block-device-host-type (host-type) => T

The host-type argument is used as the class argument in calls to
create-host. For example, the FAT file system uses the following call:

    (register-block-device-host-type :fat-host)

The file-host-mount-mixin class defines three fields: mount-args,
mount-state and mount-device. create-host is responsible for setting
all three fields and mount-host is responsible for setting the last
two fields. When create-host is successful, it should set mount-args
to a value that is useful for mount-host to identify the block device
for remounting. Usually, this is the UUID of the file system on that
block device. When create-host or mount-host are successful, they
should set mount-state to :mounted; and, set mount-device to the block
device.

Whenever a block device is unregistered, if there is a host mounted on
that block device, the mount state of that host is set to :unmounted
and the mount device is set to NIL. No further operations are
permitted on that block device by the host.

When a block device is registered, mount-host is called for each
existing host that is not mounted until a call returns T indicating
that the host was mounted on the block device.  mount-host should
probe the block device to determine if there is a file system of that
class on the block device and check to see if the UUID of that file
system matches the UUID of the host. If the UUIDs match, then the host
should be mounted on that block device. The host should assume that
the file system has been changed since the last time the host was
mounted on that block device and should re-initialize whatever
information is required.

If there is not an appropriate file system on the block device, or the
UUID does not match the UUID of the host, mount-host should not make
any changes in the host and return NIL.

If none of the existing unmounted hosts return success (T), then
create-host is called for each registered host type until a call
returns success (a host name). create-host should probe the block
device to determine if there is a file system of that class on the
block device and use the UUID of that file system to search the
name-alist to find the associated host name. If an associated host
name is found, create-host should create and initialize a host for
that block device and register the host by:

    (setf (mezzano.file-system:find-host <host name>) <host object>) => <host object>

If there is not an appropriate file system on the block device, or no
match is found in name-alist, create-host should not create a host and
return NIL.

To create a new file system on a running system. Call create-host with
the appropriate arguments. For example, to create a FAT file system host:

    (create-host :fat-host <block device> '((<uuid> <host name>)))

If the system is rebooted without saving the current state using:

    mezzano.supervisor:snapshot () => <ignored>

then the host object will be lost and will need to be recreated.

However, if the system is saved, at the start of system boot, the host
object and host name mapping will still exist and the host will be
still be mounted on the now obsolete (invalid) block device.  As the
boot process proceeds, all of the previously registered non-removable
disks and partitions are unregistered and any hosts associated with
those partitions are then marked as unmounted.  Next the currently
enumerated non-removable disks and partitions are registered as block
devices and any hosts associated with those disks and partitions are
re-mounted via calls to mount-host.

Any removable media disks and partitions that were registered when the
system was saved via snapshot are unregistered when the driver
associated with the removable media recognizes that the system was
rebooted. Again, any hosts associated with those disks and partitions
are marked as unmounted. If removable media is reinserted, any hosts
associated with those disks and partitions are remounted.

Similarly, during normal operation, when a removable media is removed,
the block devices are unregistered and the associated hosts are
unmounted. When the same media is reinserted the block devices are
registered and the associated hosts are remounted.

Block devices support the block-device APIa:

    mezzano.disk:block-device-sector-size (disk) => <sector size in bytes>

    mezzano.disk:block-device-read (disk lba n-sectors buffer &key (offset 0)) => T on success, otherwise error

    mezzano.disk:block-device-write (disk lba n-sectors buffer &key (offset 0)) => T on success, otherwise error

    mezzano.disk:block-device-flush (disk) => T on success, otherwise error

A list of block devices can obtained by calling:

    mezzano.disk:all-block-devices () => List of block devices

Block devices can be registered by:

    mezzano.disk:register-block-device (device) => Updated list of block devices

And unregistered by:

    mezzano.disk:unregister-block-device (device) => Updated list of block devices

## Future Work

Generate better error types from block devices. Perhaps add a base
condition (IO-ERROR?) that block device errors inherit from.

Do we need an unmount-host generic function which is called when a
block device is unregistered? For USB, the block device is already
unavailable, so no additional operations on the block device are
possible. But there may be some clean up of the host that is required
(other than setting the mount-state to :unmounted and the
mounted-device to NIL which already occurs)?
