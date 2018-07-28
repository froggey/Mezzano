# Creating and running Mezzano in a dual boot system.

Creating and running Mezzano in a dual boot system takes three steps:

1. Create the dual boot system

2. Create a Mezzano image

3. Put the Mezzano image on the partition in the dual boot system


### 1. Create the dual boot system.

For example, I installed Fedora 28, using custom disk partitioning:

    /	    20 GB (my development system is using about 14GB of 50GB)

    /home	    50 GB (my Mezzano development directory is using 3.5GB)

    /boot	    2 GB (increased from 1GB just because)

    /swap	    2GB (default value as system only has 2GB RAM)

    /mezzano    10 GB (created as standard partition and ext4)

    /tag-fs	    50 GB (created as standard partition and ext4 - but will change partition type later)

I created the /tag-fs partition because one of the projects I want to
work on is a Mezzano based file system. This partition is not required
otherwise required.

After installing and configuring Fedora, I noted which partitions are
/mezzano and /tag-fs by looking in /etc/fstab (in my case /mezzano was
mounted on /dev/sda3 and /tag-fs was mounted on /dev/sda5). Then I
removed the /mezzano and /tag-fs lines from /etc/fstab.

I un-mounted /mezzano and /tag-fs - they aren't going to be ext4 file
systems.

Using fdisk, I changed the partition type for /tag-fs to 7f (unkown)
as there is no tag-fs specific partition type at this time. The
partition type for /mezzano was left as 83 (Linux) which matches the
partition type in the other images generated for Mezzano.

#### kboot setup

The next step in creating a dual boot system is to update the /boot
directory by copying kboot.bin into /boot and creating kboot.cfg in
/boot.

For kboot.cfg I'm using the following contents:

    set "timeout" 5

    entry "Mezzano - hardware detection with video console" {
          set "video_mode" "lfb:1440x900"
          mezzano "hd0,2" "video-console"
    }

    entry "Mezzano - hardware detection" {
          set "video_mode" "lfb:1440x900"
          mezzano "hd0,2"
    }

    entry "Mezzano - freestanding with video console" {
          set "video_mode" "lfb:1440x900"
          mezzano "hd0,2" "freestanding" "no-detect" "video-console"
    }

    entry "Mezzano - freestanding" {
          set "video_mode" "lfb:1440x900"
          mezzano "hd0,2" "freestanding"  "no-detect"
    }

The hd0,2 is because the /mezzano partition is /dev/sda3 and kboot
uses partition numbers starting with 0 where fdisk uses partition
numbers starting with 1.

The 1440x900 is the resolution of my laptop.

"video-console" means that console messages are shown on the
display. They are not shown in a window, they are drawn directly on
the background window. This can be annoying, but useful if you are
experience crashs or panics.

"freestanding" means that Mezzano runs as is in memory, no paging and
no snapshots. This mode is suitable for read only media (eg CDROMs and
DVDs). It also requires less working drivers, so if you have
difficulting booting without "freestanding", it may be worth trying
with it.

"no-detect" disables most hardware detection. Again, this option maybe
useful when having trouble booting.

The first entry in the table is the default entry which is
automatically selected after "timeout" seconds. So, it's nice if the
first entry is the one you expect to use the most.

#### grub2 setup

I created a Mezzano boot menu item by adding the following lines to
/etc/grub.d/40_custom:


    menuentry 'Mezzano' {
    	multiboot /kboot.bin
    	module /kboot.cfg kboot.cfg
    }


After modifying /etc/grub.d/40_custom, the next step is to update
/boot/grub/grub2/grub.cfg. I suggest making a copy of this file first
so that if something goes wrong you can restore it by booting the
rescue disk and moving the original back.

To update /boot/grub/grub2/grub.cfg you need to run this command:

    sudo grub2-mkconfig -o /boot/grub/grub2/grub.cfg

This merges all of the individual grub configuration files into the
single configuration file that grub actually uses.

### 2. Create the mezzano image.

Boot Mezzano on a virtual machine and configure as desired (i.e., load
any apps or libraries), then evaluate:

    (mezzano.supervisor:snapshot)

in a lisp listener which saves the current state of Mezzano on the
virutal disk. Then, in the MBuild directory, run the shell command:

    qemu-img convert -O raw mezzano.vmdk Mezzano/tools/kboot/image.raw

which converts the vmdk file to a raw disk image. This image can be
used directly in step 3 below. However, in my configuration, image.raw
is large 4GB which makes copying it to my dual boot machine slow. So
instead, in MBuild/Mezzano/tools/kboot I run the shell command:

    ./build-native-image

which creates a live-cd version of the Mezzano image in
cdrom.iso. cdrom.iso is much smaller as `build-native-image`
compresses the Mezzano significantly.  `build-native-image` requires
that the kboot tool set be installed.


### 3. Put the Mezzano image on the partition in the dual boot system

Get the Mezzano image into the Fedora file system. Either create it
there in step 2 or copy it from where ever it was created.

If you are using cdrom.iso, then copy the image onto the partition using:

    sudo dd if=cdrom.iso of=<partition> bs=2048 skip=1635

In my case, I use of=/dev/sda3 because the /mazzano partition is
/dev/sda3. The reason I use bs=2048 and skip=1635 is that we need to
skip over the first two partitions in cdrom.iso. The output of `fdisk
-l cdrom.iso` is:

    Device     Boot  Start    End Sectors   Size Id Type
    cdrom.iso1        4488   6535    2048     1M 83 Linux
    cdrom.iso2 *      2440   4487    2048     1M 83 Linux
    cdrom.iso3        6540 542155  535616 261.5M 83 Linux

This means that the Mezzano image starts at byte offset
6540\*512. Since skip=1635 causes dd to skip over the first 1635
blocks (of size 2048) of the input file and 1635\*2048 = 6540\*512, dd
starts copying at the beginning of the Mezzano image.

If you are using image.raw, then copy the image onto the partition using:

    sudo dd if=image.raw of=<partition> bs=8192 skip=512

This works because the output of `fdisk -l image.raw` is:

    Device     Boot Start     End Sectors Size Id Type
    image.raw1       2048    4095    2048   1M 83 Linux
    image.raw2 *     4096    6143    2048   1M 83 Linux
    image.raw3       8192 8388607 8380416   4G 83 Linux

Similar to the case above, the Mezzano image starts at byte offset
8192\*512 and dd skips over the first 512 blocks (of size 8192).

Now when you reboot, one of the grub boot menu items will be "Mezzano"
and if you select that item, the kboot menu will appear and you can
select one of the versions of Mezzano boot.

### Troubleshooting

#### SMP (Symmetric Multiprocessing)

If booting fails and the last message on the console is "Multiple CPUS
detected. SMP support is currently experimental and unreliable." Then
it's likely that you are running on system with multiple processors
(including multithread cores) with SMP enabled in both HW and Mezzano
and that is failing.

This can be fixed by either turning off SMP in the BIOS (which affects
all of the OSes) or by disabling it in Mezzano.

To disable it in Mezzano, you can comment out the call to
boot-secondary-cpus in supervisor/entry.lisp (approx. line 172).

#### Disk Issues

If you suspect a disk driver issue the problem maybe the disk
controller's mode of operation. In my case, the system booted with
"freestanding" and "no-detect". But, I noticed that no disk drives
were configured ((mezzano.supervisor:all-disks) returned NIL).

Attempting to boot without "freestanding" and "no-detect" hung. The
disk controller was originally configured in the BIOS as IRRT (intel
rapid restore technology) and the other options were: ATA mode or AHCI
mode. By trial and error I determined that AHCI mode worked. All three
modes worked for my Linux partition.

ATA mode may work if it operates in ISA compatitiblity mode as the
Mezzano ATA driver only supports ISA compatible devices.

#### USB Keyboard and Mouse Issues

USB keyboards and mice are unreliable because there's no USB
stack. The BIOS emulates a PS/2 device and a lot of them tend to be
buggy. Try booting with the keyboard or mouse attached, if that doesn't work
try attaching them after booting.

In my case, the external mouse interferred with the trackpad and
internal mouse. With the external mouse plugged in, the pointer would
only move up and down near the left edge of the display, and would
only move a very short way left and right.

#### Network Issues

Mezzano only has drivers for virtio-net and rtl8168 NICs.

Last modified 17 June 2018.
