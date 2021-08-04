---
title: Pair Programming in the Pandemic
keywords: [linux, tmux, vi]
date: 2021-08-02
---

I gotta say that this pandemic has been really harsh where I live.
Most of all, I miss hanging out with my friends,
be it to chat, play something or work on any project together.
In order to adapt to this new remote world,
we had to move everything to be online.
In this spirit, lately I have been meeting (remotely) with a friend every now and then
to talk about life and work on a game as a side project[^game-side-project].

I began by teaching my friend some [Lua](https://lua.org)
and then we started building our game with [love2d](https://love2d.org).
At first, the setup was basically I sharing my screen
in Google Meet or Jitsi and he accompanying and asking questions.
Since the lectures are pretty one-sided, this worked well
but became too cumbersome when we started to make the game itself.
All the "Now edit line 32" or "Go to the definition of f and change that"
just weren't cutting it.
Furthermore, sometimes there are connection issues
and the Meet sharing screen lags a lot.

We had some ups and downs finding a good setup for programming together,
thus I started a quest in search of a better setup.
This post is a step-by-step guide written where I try to document everything
to make life easier for anyone trying to reproduce it
(myself in the future included).

[^game-side-project]: More on this on another post (someday, I hope).

## The Setup

Since both my friend and I are already Linux and (neo)vim users,
it seemed reasonable to look for a terminal based setup.

The idea is the following:
I begin by starting a shared tmux session.
Then he connects to my machine via ssh (to his own user)
and attaches to the same tmux session,
this means we both can control and interact with the same shell.
After that, we just have to navigate to the project's folder
and play with it as we wish.

Of course most of the steps above will be automatic.
And did I already mention that it is only possible 
to stay connected via ssh while the tmux session exists?

I tested this with my machine running Arch Linux as host
and my friend's machine running Ubuntu 20.04 or Ubuntu WSL.
Therefore the commands below will always assume `pacman`
is the system's package manager
and that `systemd` is installed and in charge of the services.
But it should be easy adapt it to any other Linux host[^adapt-host].
For the guest, any system with a ssh client should do.

:::Note
Note: This guide also assumes you have root access on your machine.
:::

[^adapt-host]: Probably, all you have to do is
change `pacman` for your system's package manager (`apt`, `yum`, `xbps` etc.),
maybe change the location of a couple configuration folders
and substitute `systemctl` by whatever service manager you may use.

## Configuration steps (on host)

### Install everything

We begin by ensuring everything we need is installed.
On the host system we need a ssh server
as well as tmux for sharing the session.

```sh
sudo pacman -S openssh tmux
```

Since I travel a lot and can't always guarantee that
I will have admin access to my current router,
I'm also using `ngrok` for tunnel reversing.
I must admit that I don't like letting a third party handle this part
and would be much more comfortable by using my own VPN our some other solution.
But since I don't want to spend any money on this
and ngrok free plan attends my needs pretty well,
the setup will stay like this until I can think of a clever (and cheap) way to do this part.

Therefore, just follow the instructions on [ngrok's site](https://ngrok.com/product),
sign up for an account, download it and ngrok will be ready to go.
Also notice that if you are pair programming in a LAN, there is no need for ngrok
as you may simply ssh using the private IPs.
In our case, we are a couple states apart so some kind of port forwarding or reverse tunneling is needed.

Also ensure that you have a terminal based text editor: neovim, vim, emacs, nano, ed etc.

### Setup an user for the remote client

I must admit that I don't really like giving ssh permissions for my personal user.
Even though I trust my friends[^robbery],
it is usually better to let them only play with a sandboxed part of my machine.
On the other side, when we are pair programming,
I want to let them have access to my files.
The steps below are a nice way to retie those two opposite goals.

We begin by creating an user and giving it a password.
I will call it `frienduser` but you may name it whatever you like.

```sh
sudo useradd -m frienduser
sudo passwd frienduser
```

[^robbery]: I don't trust someone who may steal their machine and ssh into mine though.

### Setup a group with both your user and the new user

Now we create a group for both our users.
I will call it `tmux`, perhaps unimaginatively,
but, again, you can call it whatever you want.
Be sure to add your own user and the newly created user to the group.

```sh
sudo groupadd tmux
sudo usermod -a -G tmux youruser
sudo usermod -a -G tmux frienduser
```

### Time to call your friend

This is the only step on the setup that you will need to call your friend.
Send him a message asking for his public key.

Of course, if he is not as tech savvy as you,
it is possible that he has no idea what this means.
Then you must tell him to also guarantee that he has ssh installed
and send you the content in the file `~/.ssh/id_rsa.pub`.
If it does not exist, he must run the command

```sh
ssh-keygen
```

And follow the instructions there.
Setting a password for his private key when queried is also a good idea.

Now the file `~/.ssh/id_rsa.pub` should exist in his machine
and he can send you the content.

:::Note
Note: I feel I shouldn't have to mention this but for the sake of comprehensivity
I will...
The command `ssh-keygen` creates two files,
a private key (`id_rsa`) and a public key (`id_rsa.pub`).
One should **never** send the private key.
:::

### Authorize your friend's key

Now that you have the key at hand,
you must let ssh know of it.
The ssh server looks for authorized keys for a given user on the file
`$HOME/.ssh/authorized_keys`.
Thus we must create this file and add the key to it on frienduser's home folder.

```sh
sudo -u frienduser mkdir -p /home/frienduser/.ssh
sudo -u frienduser echo -n 'command="tmux -u -S /tmp/sharedtmux attach -t gamemaking" ' >> /home/frienduser/.ssh/authorized_keys
sudo -u frienduser echo $PUBLIC_KEY >> /home/frienduser/.ssh/authorized_keys
```

The middle line may seem weird at first.
It means that your friend's connection
will automatically put him in the right shared tmux session.
This is discussed more when
[we create the shared tmux session](#create-the-shared-tmux-session).
As a positive side effect,
the connection will only be accepted if the session exists.

### Extra step: configure the SSH Server

This section is totally optional and is here just to added security.
On some distros, the OpenSSH server defaults to only accept
connections using key pairs while others will accept logins by password.
Here we will see how to configure ssh require both a key _and_ a password.

OpenSSH stores its server configuration in the file `/etc/ssh/sshd_config`[^user-cfg].
Just open it with your favorite editor and add the following line to the end:

    AuthenticationMethods "publickey,password"

Now the server will ask for both a key and a password before allowing login.
There are also other options for authentication methods besides there two.
For a full list, take a look at the
[man page for `sshd_config(5)`](https://man.openbsd.org/sshd_config#AuthenticationMethods).

Also know that these changes only apply after you
[restart the service](#start-the-ssh-server).

[^user-cfg]: Beware to not mix this up with `/etc/ssh/ssh_config`,
the file where OpenSSH stores the _client_ configuration.

### Extra step: configure tmux for multiple clients

When multiple clients connect to the same tmux session,
it will by default resize the screen on each command
to the same of the last client to do something.
I personally find all this resizing pretty annoying and prefer to
configure tmux to stick with a size that fits all connected clients.
All you have to do is edit `~/.tmux.conf` and add the following lines:

    set -g window-size smallest
    setw -g aggressive-resize on

Now tmux will always use the smallest size among all clients,
so it is a good idea to tell everybody working on this setup
to use their terminals on fullscreen.

## Start Pair Programming (you)

### Start the SSH Server

You should start by guaranteeing that the ssh server daemon
is running.
In Arch (or any other systemd based distro),

```sh
systemctl status sshd
```

If the service is inactive or dead, start it with

```sh
systemctl start sshd
```

You will be queried for your password and then it is ready to go.

:::Note
I also tested this in WSL2[^wsl]
but in there Ubuntu does not use systemd
(maybe for some system incompatibilities?).
If you are following this tutorial from WSL2, the respective commands are

```sh
sudo service ssh status
sudo service ssh start
```
:::

[^wsl]: I know what you're thinking... But I really need Windows for ~~gaming~~ work.

### Create the shared tmux session

We will store the shared session in a temporary file
that can be accessed both by your and your friend's user.
First create the session with a special socket file:

```sh
tmux -S /tmp/sharedtmux new-session -s gamemaking -d
```

Here I am calling the session `gamemaking`
and storing the socket in `/tmp/sharedtmux`
but, again, you can give them whatever name you want.
I also decided to use a temporary file because I prefer these
sockets to be disposable.
However you may prefer to store it to have some kind of persistence on
your session's layout and open programs.
The only important thing in here is that the folder should be visible
for all user in the `tmux` group.

Now we allow both users to read and write this file,

```sh
chgrp tmux /tmp/sharedtmux
chmod g+rw /tmp/sharedtmux
```

This way, both of them will have total control of the tmux process.

### Start ngrok tunnel

If you followed the steps on ngrok's manual,
it should suffice to run a tcp connection on port 22
(This is ssh's default port).

```sh
ngrok tcp 22
```

When the TUI starts, there will be a line looking something like

    Forwarding          tcp://2.tcp.ngrok.io:15727 -> localhost:22

The `2.tcp.ngrok.io` part is your assigned hostname
(it is always an arbitrary number followed by `.tcp.ngrok.io`)
the number after the colon, `15727` in this example, is the port.
You should copy both the hostname and port somewhere and send it to your friend.
These are redirecting directly into your machine.

:::Note
An important note: you must leave ngrok's window open.
If you exit it, the process will be killed and the tunnel closed.
:::


### Attach to the shared session

Now, in a new terminal you can attach to the tmux session
and wait for your friend to connect.

```sh
tmux -u -S /tmp/sharedtmux attach -t gamemaking
```

## Start Pair Programming (friend)

Provided your friend's key is already setup,
all you have to do is copy the ngrok port and hostname
and send it.
Then, connecting should be as simple as running something like

```sh
ssh -t -p PORT frienduser@HOST
```

Now you're ready to go!
Have fun programming together!

## Extra Step: Query ngrok info from command line

Although the setup above works,
there is something in it that really bothers me:
you have to manually look at ngrok's TUI and copy the hostname and port form it.
C'mon, we're in a Linux shell, the land of automation!
Of course there is some better way to do that.

I read and reread ngrok's help pages but the binary there doesn't seem to
directly ask the binary for this information in any way.
Luckily, I stumbled on
[this gist](https://gist.github.com/rjz/af40158c529d7c407420fc0de490758b)
and [this blog post](https://queenofdowntime.com/blog/remote-pair-programming)
while trying to circumvent it and good news: there is a way!

Apparently ngrok exposes its tunneling information
on `localhost:4040` in JSON format.
Thus we can query it  by doing

```sh
curl -s http://localhost:4040/api/tunnels \
  | sed -nE 's|.*"public_url":"tcp://([^:]*):([0-9]*)".*|\1 \2|p'
```

This way, we store the tunnel's host and port at the variables
`ngrok_host` and `ngrok_port`.

## Extra Step: Turn all that into a script

Now that we solved the last bit of manual work,
we can put everything together in a script
that generates a shared tmux session,
tunnels it with ngrok and tells you how to remotely connect to it.
Below is a script based on the one found at the end of
[this guide](https://queenofdowntime.com/blog/remote-pair-programming).
Also notice that if `sshd` service is inactive,
you will need the root password to start it.

```sh
#!/usr/bin/env bash

# Read parameters from command line arguments
# or use the same defaults as this post
frienduser="${1:-frienduser}"
tmux_file="${2:-/tmp/sharedtmux}"
tmux_session="${3:-gamemaking}"
tmux_group="${4:-tmux}"

# If sshd is inactive, begin by starting it
systemctl status sshd.service >/dev/null
if [[ $? -gt 0 ]]; then
  sudo systemctl start sshd.service
fi

# Create shared tmux session
tmux -S "$tmux_file" new-session -s "$tmux_session" -d
# Assign right permissions to group
chgrp $tmux_group "$tmux_file"
chmod g+rw "$tmux_file"

# Start ngrok using the same tmux socket file
# but in a different session.
# This ensures the ngrok TUI will run non-blocking.
tmux -S "$tmux_file" new-session -s ngrok -d
tmux -S "$tmux_file" send -t ngrok "ngrok tcp 22" ENTER
# Wait a little while ngrok starts
sleep 2

# Query ngrok for host and port
ngrok_url='http://localhost:4040/api/tunnels'
query_ngrok() {
  local ngrok_host ngrok_port
  read ngrok_host ngrok_port <<< $(curl -s $ngrok_url \
    | sed -nE 's|.*"public_url":"tcp://([^:]*):([0-9]*)".*|\1 \2|p')
  echo "ssh -t -p $ngrok_port $frienduser@$ngrok_host"
}

# Echo the proper ssh command to tmux
tmux -S "$tmux_file" send -t "$tmux_session" "echo $(query_ngrok)" ENTER
# And also copy it to X clipboard for convenience
if command -v xclip &> /dev/null; then
  query_ngrok | xclip
fi

# Attach to the shared session
tmux -u -S "$tmux_file" attach -t "$tmux_session"
```

## References

1. [Using SSH and Tmux for screen sharing](https://www.redhat.com/sysadmin/ssh-tmux-screen-sharing)
2. [How to pair-program remotely and not lose the will to live](https://queenofdowntime.com/blog/remote-pair-programming)
3. [ssh, tmux and vim: A Simple Yet Effective Pair Programming Setup](https://ptc-it.de/pairing-with-tmux-and-vim/)
4. [rjz/ngrok_hostname.sh](https://gist.github.com/rjz/af40158c529d7c407420fc0de490758b)
