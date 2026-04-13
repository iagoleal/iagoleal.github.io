---
title: Encrypting an external device with LUKS
keywords: [linux, encryption, LUKS, cryptsetup, dm-crypt, usb drive, udiskie, udisks2, automation]
date: 2026-03-24
theme: workflow
description:
  A tutorial on how to wipe, encrypt, auto mount and decrypt an external usd drive on linux.
---

I, like many other folks out there,
use a small USB drive to store personal documents and other files.
It works as a useful backup in case my system fails or gets stolen.
However, since I am always traveling this has always made me nervous,
because it is too easy to lose a drive the size of a coin.

As I only ever plug it into a Linux machine (I already keep it formatted as `ext4`),
it seems sensible to encrypt it with LUKS.
Also, it is annoying to write a new password all the time,
so --- since my threat model considers my laptop to be safe and only accessible by me ---
let's also setup the device to auto decrypt whenever plugged on it.

**Important**: Throughout this post,
we will use the dummy `/dev/sdX` to refer to the device descriptor.
Make sure to check right name for your setup.
Also, guarantee that the device is unmounted,
specially if you use an automount daemon like udiskie.

Wipe It Out
===========

First of all let's substitute everything
This step is optional,
but useful if you are already storing something in the device.

Let's shred this device to zeros.

```sh
dd if=/dev/zero of=/dev/sdX status=progress bs=1M
```
You can also use `/dev/urandom` if you want,
but writing zeros is faster and later, when encrypted, this will look like random data.
You can check the [Arch Wiki](https://wiki.archlinux.org/title/Securely_wipe_disk)
for other methods of wiping a disk.

Encrypt Drive
=============

If you didn't follow the previous step,
do keep in mind that this one is not optional and it will _delete everything_ in the drive.
Backup accordingly.

We are using the Linux kernel's support for full-disk encryption
via `dm-crypt`.
We users interact with it using the `cryptsetup` command.
Let's use it to format the whole drive using LUKS2 headers[^luks2].

```sh
sudo cryptsetup luksFormat /dev/sdX --type luks2 --label drive
```

[^luks2]: LUKS2 is the most recent LUKS (Linux Unified Key Setup) version as of writing.

You will be prompted for a password when running this command.
For now, this is the only way to access the data in this device.
Make sure to remember it.
Also, the label `drive` can be anything you want.

Now let's open the drive
and assign a name to it.
```sh
sudo cryptsetup open --type luks2 /dev/sdX drive-name
```
It will prompt you for the password and map the _decrypted_ device to
`/dev/mapper/drive-name`.

I am going to format it with as `ext4` because it is fast, stable,
and the encryption scheme already only works on Linux.
But VFAT or almost any other should work well enough.

```sh
sudo mkfs.ext4 /dev/mapper/drive-name
```

Now your drive is ready to use!
You can close and remove it if you want.

```sh
sudo cryptsetup close drive-name
```

Whenever you want to use it,
you will have to first "open" the drive (decrypt) and only then mount its partition.
Notice that the mapped name can be anything you want.

```sh
sudo cryptsetup open --type luks2 /dev/sdX usb
sudo mkdir -p /media/usb
sudo mount /dev/mapper/usb /media/usb
```


Auto-Opening and Mounting
=========================

This whole opening and closing, mounting and unmounting dance
can get tiresome pretty quickly.
If you trust a machine,
you can let it decrypt your USB drive automatically.

Keyfile
-------

Till now, our device only opens with a password.
Let's allow it to also be opened using a **keyfile**.
This can be any file you want: random bytes, a giant riddle written in a txt, your dog's picture, etc.
For the sake of simplicity and entropy,
we are going with `4kB` of randomness.

```sh
dd if=/dev/urandom of=/path/to/keyfile bs=1024 count=4
```

Remember that `/path/to/keyfile` must be somewhere safe,
or it defeats the whole purpose.
If you have an encrypted home folder, you can keep it as a dotfile in there, for example.

Allow the keyfile to open your device.

```sh
sudo cryptsetup luksAddKey /dev/sdX /path/to/keyfile
```

You can verify that it worked by checking that there are two keyslots
in the output of
(one for the password, and another for the keyfile)

```sh
sudo cryptsetup luksDump /dev/sdX
```

Now you can open without a password!

```sh
sudo cryptsetup open --type luks2 --key-file /path/to/keyfile /dev/sdb usb
```

Automount with `udiskie`
------------------------

If you have an automount setup,
it is necessary to instruct it on where to find the keyfile.

I will show it for [udiskie](https://github.com/coldfix/udiskie)
because that is what I use and because I don't want to mount the drive at boot,
but at user level[^crypttab]
I imagine it is similar for any other setup though.

[^crypttab]: If you do want to unlock and mount it at boot,
editing `/etc/crypttab` is an elegant, dependency-free solution.


Start by checking the device's UUID,

```sh
lsblk -ndo UUID /dev/sdX
```

And proceed to create or edit `$HOME/.config/udiskie/config.yml`
to link this UUID to the keyfile.
You must write a section like this:

```yaml
device_config:
- id_uuid: <DEVICE UUID>
  keyfile: /path/to/keyfile
```

Keep in mind that the keyfile must be readable by the user running `udiskie`.

Conclusion
==========

After spending some time searching for encryption solutions,
I ended up with this one, which leaves most of the magic to the Linux kernel itself.

Below is a small proof-of-concept script automating the whole process.
Make sure to edit it when adapting to your workflow.


```sh
#!/bin/sh
# This script requires root access to work.
# Use it by passing the device name and an optional keyfile name.
#   usb-encrypt "/dev/sdX" "/path/to/keyfile"

DEVICE=$1
KEYFILE=$2
NAME=$(uuidgen)

# Wipe the drive
sudo dd if=/dev/urandom of="$DEVICE" bs=4096 iflag=fullblock status=progress

# Encrypt and format
sudo cryptsetup luksFormat --type luks2 --label drive "$DEVICE"
sudo cryptsetup open --type luks2 "$DEVICE" "$NAME"
sudo mkfs.ext4 "/dev/mapper/$NAME"

# Create and assign keyfile
if [ -n "$KEYFILE" ] ; then
  sudo dd if=/dev/urandom of="$KEYFILE" bs=1024 count=4
  sudo cryptsetup luksAddKey "$DEVICE" "$KEYFILE"

  # Configure udiskie if the executable exists
  if command -v udiskie >/dev/null 2>&1 ; then
    UUID="$(lsblk -ndo UUID "$DEVICE")"
    cat >>"${XDG_CONFIG_HOME:-$HOME/.config}/udiskie/config.yml" <<EOF
device_config:
- id_uuid: $UUID
  keyfile: $KEYFILE
EOF
  fi
fi

cryptsetup close "$NAME"
```


Reference
=========

- [cryptsetup's man page](https://man.voidlinux.org/cryptsetup);
- [The one and only Arch wiki entry on dm-crypt](https://wiki.archlinux.org/title/Dm-crypt).
