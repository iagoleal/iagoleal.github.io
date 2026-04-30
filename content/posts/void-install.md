---
title: Encrypted Void Linux Installation Booting From EFI Stub
keywords: [linux, encryption, LUKS, cryptsetup, hibernation, efistub, swapfile, keyfile]
date: 2026-04-29
theme: workflow
description:
  How to install Void Linux with LVM over LUKS for full-disk encryption,
  EFI Stub as bootloader, hibernation, and per-user encryption.
---

Recently I changed my notebook's distro from Arch to [Void Linux](https://voidlinux.org/).
Since I wanted some special features that the provided installer cannot handle yet,
I had to go with the good ol' chroot method.
These are the captain's logs documenting the whole process.
It exists in case I ever decide to do this installation again,
or for possibly helping someone out who's following the same path.


This post describes how to do a fresh installation with the following features:

* Full disk encryption with LVM on LUKS.
* No bootloader: system boots directly from EFI Stub.[^unencrypted-efi]
* Hibernation via (encrypted) swapfile.
* Passwordless decryption using keyfile on USB Drive.
* Per-user home folder encryption via fscrypt.


[^unencrypted-efi]: For simplicity, this setup uses an unencrypted bootloader.
You can encrypt it in case that's incompatible with your threat model,
but be warned that the setup becomes much more convoluted.

Adjust installation environment
===============================

Login as `root` so you don't have to type `sudo` all the time.

Let's setup a keyboard layout and wi-fi connection for the installation.

Check all available keymaps and choose the appropriate one for your system.

    find /usr/share/kbd/keymaps/ -type f -iname '*.map.gz'

Then load it without the `.map.gz` suffix. For example,

    loadkeys br-abnt2

The installer comes with `wpa_supplicant`.
Use its CLI to setup an wi-fi connection.

TODO
TODO
TOD

Partitioning
============

On what follows,
I refer to the disk where we are installing the system as `/dev/nvme0n1`.
Adapt it to the appropriate device descriptor in your configuration (`/dev/sda`, `/dev/sdb`... `lsblk` is your friend here.)

The scheme consists of two partitions:
one (unencrypted) for the EFI bootloader and another (encrypted) for the rest.
LVM then takes care of further partitioning.

Run `cfdisk` and create a GPT table and a (at least) 1G partition of type `EFI System`,
allocating the rest for a `Linux filesystem` partition.
You should get something like the below,


    Device           Start       End   Sectors  Size Type
    /dev/nvme0n1p1    2048   2099199   2097152    1G EFI System
    /dev/nvme0n1p2 2099200 500117503 499908303  238G Linux filesystem

In most tutorials out there,
the recommendation is to allocate a very small EFI partition.
Generally, from 100M to 512M.
In our case, however, using no bootloader produces a quite large initramfs (~150M).
Since Void does not automatically clean `/boot` after a Kernel upgrade,
I recommend playing it safe to prevent future headaches.

The first partition will contain our kernel and
(from the EFI spec) must be formatted as FAT32.
Also, in this setup, we leave this one unencrypted.

    mkfs.fat -F 32 -n EFI /dev/nvme0n1p1


Disk encryption
---------------

Now it's time to encrypt the remainder of the drive.
Use `cryptsetup` to format the second partition as LUKS2 and leave it open.

    cryptsetup luksFormat /dev/nvme0n1p2 --type luks2 --label luks
    cryptsetup open --type luks /dev/nvme0n1p2 vault

The command will prompt you for a password.
It is _very important_ that you remember this password,
or you'll get locked out of your system.
Also, keep in mind that the encryption is only as safe
as the strength of this password.


Logical Volume partitioning
---------------------------

We use LVM to manage the partitions inside the encrypted storage.
Begin by creating a volume group (with the unimaginative name `void`)

    vgcreate void /dev/mapper/vault

If using a Swap partition, create and activate it.

    lvcreate --name swap -L 8G void
    mkswap /dev/void/swap
    swapon /dev/void/swap

I personally go with a [swapfile](#swapfile).
jjjjjjj
Create volumes for `/` and `/home/`,

    lvcreate --name root -L 100G void
    lvcreate --name home -l 100%FREE void

Notice the `-L` and `-l` flags!
One is required for absolute sizes while the other is for relative ones.

For the filesystems, I'm going with the classic ext4.
You can use any other Linux-supported filesystem in here.

    mkfs.ext4 -L root /dev/void/root
    mkfs.ext4 -L home -O encrypt /dev/void/home

You only need to active the `-O encrypt` flag when
doing [per-user home folder encryption](#fscrypt).

After this, your disk should look like

    $ lsblk /dev/nvme0n1 -o NAME,SIZE,TYPE,FSTYPE
    NAME               SIZE TYPE  FSTYPE
    nvme0n1          238.5G disk
    ├─nvme0n1p1          1G part  vfat
    └─nvme0n1p2        238G part  crypto_LUKS
      └─vault          238G crypt LVM2_member
        ├─void-root    100G lvm   ext4
        └─void-home    138G lvm   ext4

System Installation
===================

Mount all partitions/volumes to the appropriate subdirectories of `/mnt`,

    mkdir -p /mnt
    mkdir -p /mnt/home
    mkdir -p /mnt/boot
    mount /dev/void/root /mnt
    mount /dev/void/home /mnt/home
    mount /dev/nvme0n1p1 /mnt/boot

For package installation,
first copy the XBPS RSA keys from the live CD to the mounted root directory.

    mkdir -p /mnt/var/db/xbps/keys
    cp /var/db/xbps/keys/* /mnt/var/db/xbps/keys/

Install the base system and necessary packages for this setup,

    xbps-install -Sy -R https://repo-default.voidlinux.org/current -r /mnt \
      base-system lvm2 cryptsetup efibootmgr fscrypt

Generate system's `/etc/fstab`,

    xgenfstab -U /mnt > /mnt/etc/fstab

The `-U` flag is because I, personally, prefer to use UUIDs instead of kernel descriptors.

Enter chroot at `/mnt` to complete the installation.
Void uses `dash` as `/bin/sh`, but I prefer `bash` or `zsh` for iterative editing.

    xchroot /mnt /bin/bash


Inside the chroot
=================

Everything from now on will happen inside the `chroot`,
thus you have no access to external packages from the Live CD.

Configure the root user,

    chown root:root /
    chmod 755 /
    passwd root

Set a hostname,

    echo <YOUR_FAVORITE_HOSTNAME> > /etc/hostname

Configure locale for glibc,

    echo "LANG=en_US.UTF-8" > /etc/locale.conf
    echo "en_US.UTF-8 UTF-8" >> /etc/default/libc-locales
    xbps-reconfigure -f glibc-locales


Bootloader
==========

Void ships with a kernel hook in `/etc/default/efibootmgr-kernel-hook`
for configuring efibootmgr after every update.
Let's edit it

    vi /etc/default/efibootmgr-kernel-hook

We need to write proper options,
so the kernel can handle decrypting the filesystem
as well as hibernating from the swapfile.

```conf
    # Options for the kernel hook script installed by the efibootmgr package.
    # To allow efibootmgr to modify boot entries, set
    MODIFY_EFI_ENTRIES=1

    # Kernel command-line options.  Example:
    OPTIONS="loglevel=4 rd.md=0 rd.dm=0 \
      rd.luks.uuid=4581f162-f661-48f6-aaa4-e2355f06aa6b \
      rd.lvm.lv=void/root rd.lvm.lv=void/home \
      root=/dev/mapper/void-root rootfstype=ext4 rootflags=rw,relatime \
      rd.luks.key.tout=5 rd.luks.key=/chave:UUID=d40ec382-7984-4d36-9a37-46866ed5610a \
      resume=/dev/mapper/void-root resume_offset=12818432"

    # Disk where EFI Partition is.  Default is /dev/sda
    DISK="/dev/nvme0n1"

    # Partition number of EFI Partition.  Default is 1
    PART=1
```

Bonus: Swap file {#swapfile}
----------------

    mkswap -U clear --size 8G --file /swapfile
    swapon /swapfile

Edit `/etc/fstab` and add the line

    /swapfile             none            swap        defaults      0 0

Bonus: USB Keyfile
------------------

The manpage for `dracut.cmdline(7)` is very well-written,
so we are basically following what they recommend in there.

Let's mount the pendrive to store the keyfile.
I just recommend not using `/mnt` because we will need it
for the root filesystem in the next step.
Supposing the device in `/dev/sdX` and the filesystem is in its first partition,

    mkdir -p /media/usb
    mount /dev/sdX1 /media/usb

We create a random 4 Kb keyfile for entropy,
but you can use absolutely any file you want (GPG key, family photo, etc.)

    dd if=/dev/urandom of=/media/usb/keyfile bs=4096 count=1

Now add it to the LUKS encrypted partiton.

    cryptsetup luksAddKey /dev/nvme0n1p2 /media/sd128/keyfile

You can verify that it works with the `luksDump` command.
Check that there are two keyslots in the output
(one for the password, and another for the keyfile).

    cryptsetup luksDump /dev/nvme0n1p2 | grep "Keyslots:" -A20

Finally, edit `/etc/default/efibootmgr-kernel-hook`
to add the respective kernel parameters.
There are two important to us, `rd.luks.key` and `rd.luke.key.tout`.
The first takes the form

    rf.luks.key=<path to key>:<device where it is stored>

The path is an absolute path from the device's filesystem root,
whereas the device can specified in any of the usual ways,
such LABEL or UUID.
For example, if its UUID is `<UUID>`, you should write,

    rf.luks.key=/keyfile:UUID=<UUID>

The second parameter establishes how long the kernel
will search for the keyfile before giving up and asking for a password.
For a 5s timeout,

    rf.luks.key.tout=5

Note that if you leave it a `0` (the default),
it will never ask for the password.

Reconfigure the kernel
----------------------

Check your current linux kernel version with `xbps-query linux`.
It will appear in `run_depends` as a package `linux<major>.<minor>`.

    xbps-reconfigure -f linux<major>.<minor>


User creation with filesystem encryption
========================================

If you're not much of a bash person,
begin by installing your favorite shell.

    xbps-install -S zsh

Let's add a non-root user with a zsh default shell and some useful groups.
See the [documentation(https://docs.voidlinux.org/config/users-and-groups.html)
for all default groups in Void.

    useradd iago:iago             \
        --create-home               \
        --shell /usr/bin/zsh        \
        --groups wheel,users,audio,video
    passwd iago

Run (some variation of) the above for each user you want in the new machine.

You can also change the shell later with

    chsh -s <shell> <user_name>

Allow sudo for wheel group
--------------------------

Run `visudo` and uncomment the line

    #%wheel ALL=(ALL) ALL


Encrypt home folders {#fscrypt}
--------------------

    xbps-install -S fscrypt


Complete installation
=====================

    exit
    umount -R /mnt
    reboot


References
==========

* [Void installation docs](https://docs.voidlinux.org/installation/guides/fde.html);
* Matthias Totschnig's post on [Booting Void Linux using the EFI boot stub](https://mth.st/blog/void-efistub/);
* The Arch wiki entry on [Encrypting an entire system](https://wiki.archlinux.org/title/Dm-crypt/Encrypting_an_entire_system).
  (but watch out for  `systemd` specific stuff)
