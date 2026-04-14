---
title: Encrypted Void Linux Installation Booting From EFI Stub
keywords: [linux, encryption, LUKS, cryptsetup, hibernation, efistub, swapfile, keyfile]
date: 2026-04-03
theme: workflow
description:
---

The document describes how to do a fresh [Void Linux](https://voidlinux.org/)
installation with the following features:

- Full disk encryption with LVM on LUKS;
- No bootloader: system boots directly from EFI Stub;
- Hibernation via (encrypted) swapfile;
- Passwordless decryption using keyfile on USB Drive.
- Per-user home folder encryption;

Adjust installation environment
===============================

Login as `root` so you don't have to type `sudo` all the time.

Proper keyboard setup


Check all available keymaps and choose the appropriate one for your system.

    $ find /usr/share/kbd/keymaps/ -type f -iname '*.map.gz'

In my case, it is

    # loadkeys br-abnt2

Setup wi-fi connection.


Partitioning
============

Two partitions.

    Device           Start       End   Sectors  Size Type
    /dev/nvme0n1p1    2048   1050623   1048576  512M EFI System
    /dev/nvme0n1p2 1050624 500117503 499066880  238G Linux filesystem


First partition must be FAT32

    # mkfs.fat -F 32 -n EFI /dev/nvme0n1p1

This is the EFI Stub partition that will contain our kernel.
In this setup, we leave this one unencrypted.
You can encrypt it in case this does not fit you threat model,
but be warned that the setup becomes much more convoluted.

LUKS
----

    # cryptsetup luksFormat /dev/nvme0n1p2 --type luks2 --label luks
    # cryptsetup open --type luks /dev/nvme0n1p2  void


LVM
---

    # vgcreate void /dev/mapper/void



If using Swap partition

    # lvcreate --name swap -L 8G void

In general

    # lvcreate --name root -L 100G void
    # lvcreate --name home -l 100%FREE voi


Notice the -L and -l flags!

Create the filesystems

    # mkfs.ext4 -L root /dev/void/root
    # mkfs.ext4 -L home -O encrypt /dev/void/home

If using a SWAP partition

    # mkswap /dev/void/swap
    # swapon /dev/void/swap


System Installation
===================

Mount all partitions/volumes on the appropriate subdirectories of `/mnt`

    # mkdir -p /mnt
    # mkdir -p /mnt/home
    # mkdir -p /mnt/boot
    # mount /dev/void/root /mnt
    # mount /dev/void/home /mnt/home
    # mount /dev/nvme0n1p1 /mnt/boot

For package installation, first copy the XBPS RSA keys from the live CD to mounted root directory.

    # mkdir -p /mnt/var/db/xbps/keys
    # cp /var/db/xbps/keys/* /mnt/var/db/xbps/keys/

Install base system and necessary packages

    # xbps-install -Sy -R https://repo-default.voidlinux.org/current -r /mnt \
        base-system lvm2 cryptsetup efibootmgr

Generate system's `/etc/fstab`

    # xgenfstab -U /mnt > /mnt/etc/fstab

The flag `-U` is because personally prefer to use UUIDs.


Enter chroot at `/mnt`.
Void uses `dash` as `/bin/sh`, but I prefer to `bash` for iteractive editing.
Everything from now on will happen inside the `chroot`.

    # xchroot /mnt /bin/bash

Configure root user,

    # chown root:root /
    # chmod 755 /
    # passwd root

Set hostname

    # echo {YOUR_FAVORITE_HOSTNAME} > /etc/hostname

Configure locale for glibc,

    # echo "LANG=en_US.UTF-8" > /etc/locale.conf
    # echo "en_US.UTF-8 UTF-8" >> /etc/default/libc-locales
    # xbps-reconfigure -f glibc-locales


Bootloader
==========

Void ships with a kernel hook in `/etc/default/efibootmgr-kernel-hook`
for configuring efibootmgr after every update.
Let's edit it

    # vi /etc/default/efibootmgr-kernel-hook

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

Bonus: Swap file
----------------

    # mkswap -U clear --size 8G --file /swapfile
    # swapon /swapfile

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

    # mkdir -p /media/usb
    # mount /dev/sdX1 /media/usb

We create a random 4 Kb keyfile for entropy,
but you can use absolutely any file you want (GPG key, family photo, etc.)

    # dd if=/dev/urandom of=/media/usb/keyfile bs=4096 count=1

Now add it to the LUKS encrypted partiton.

    # cryptsetup luksAddKey /dev/nvme0n1p2 /media/sd128/keyfile

You can verify that it works with the `luksDump` command.
Check that there are two keyslots in the output
(one for the password, and another for the keyfile).

    # cryptsetup luksDump /dev/nvme0n1p2 | grep "Keyslots:" -A20

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

    # xbps-reconfigure -f linux<major>.<minor>


User creation with filesystem encryption
========================================

If you're not much of a bash person,
begin by installing your favorite shell.

    # xbps-install -S zsh

Let's add a non-root user with a zsh default shell and some useful groups.
See the [documentation(https://docs.voidlinux.org/config/users-and-groups.html)
for all default groups in Void.

    # useradd iago:iago             \
        --create-home               \
        --shell /usr/bin/zsh        \
        --groups wheel,users,audio,video
    # passwd iago

Run (some variation of) the above for each user you want in the new machine.

You can also change the shell later with

    $ chsh -s <shell> <user_name>

Allow sudo for wheel group
--------------------------

Run

    # visudo

And uncomment the line

#%wheel ALL=(ALL) ALL


Encrypt home folders
--------------------

    # xbps-install -S fscrypt


Complete installation
=====================

    # exit
    # umount -R /mnt
    # reboot


References
==========

* https://docs.voidlinux.org/installation/guides/fde.html
* https://mth.st/blog/void-efistub/
