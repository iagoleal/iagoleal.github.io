---
title: Running Jupyter Notebooks from a Supercomputer
keywords: [linux, jupyter, ssh]
date: 2025-04-06
description:
  A simple ssh workflow for interacting
  with Jupyter notebooks from your browser
  while using  an HPC supercomputer's resources.
---

When running a program in an HPC cluster,
the most common workflow is to prepare a launcher script and send it to the cluster's job scheduler.
This is great for non-interactive code,
but what to do when the code you are running consists of a server you need to interact with?
For example, many people write machine learning code using Jupyter notebooks,
which require starting a server and then manually accessing them.

Recently I had to use remote GPUs to run some ML notebooks and decided to document my efforts.
If you found this post and are perhaps dealing with the same headaches, have no fear!
By doing a couple ssh tunnels,
you will be in no time running using supercomputer resources from the comfort of your own browser.

As I am using [Purdue's Anvil supercomputer](https://www.rcac.purdue.edu/anvil),
this post will focus on it.
Nevertheless, any cluster doing its workload management
through [SLURM](https://slurm.schedmd.com/overview.html) should work the same.

Launching the Server
====================

First things first, let's connect to the cluster via SSH.
I will use `<USERNAME>` to refer to your remote user (on the cluster)
and `<ALLOCATION>` for the allocation code giving access to your resources.

```sh
ssh -l <USERNAME> anvil.rcac.purdue.edu
```

The cluster will connect you to a _login node_ from where you can
prepare and schedule your jobs.
By checking your `hostname`,
it should start with `login` and some digits.
For example, on Anvil I get

    $ hostname
    login05.anvil.rcac.purdue.edu

The usual SLURM workflow consists of writing a script for the application
and launching it with `sbatch`.
In our case, the job is interactive,
so we use its cousin `sinteractive` to start an interactive section on a _compute node_.
You call it by passing your allocation and specifying the resources needed for the session.

```sh
sinteractive -A <ALLOCATION> --partition gpu --nodes=1 --gpus-per-node=1 --time=3:00:00
```

It may take a while until the cluster assigns you the needed resources.
When it is ready, you will be back at a shell but within a compute node.
You can check that the `hostname` changed. On Anvil I get

    $ hostname
    a240.anvil.rcac.purdue.edu

Now we are ready to get working!
Load the modules and start the Jupyter server at a chosen port.
Let's use `8895` for didactic purposes. You can choose whatever you prefer.
We also tell it to do not start a web browser (no use on a remote machine)
and to serve publicly by assigning an ip `0.0.0.0`.


```sh
module load jupyter

jupyter notebook --port 8895 --no-browser --ip=0.0.0.0
```

At the end of its output,
Jupyter will show a local url with a token like

    http://127.0.0.1:8895/?token=deee923fe443decacc7ae4a127dc0a1bbe0a65b96d91e7da

Take note of it.

The server is running and we can interact with it from Anvil!

Connecting From Your Browser
============================

Although we can already use the supercomputer resources to run the notebooks,
we are still missing a way to interact with it from our own machines.
The solution is to connect again to the cluster while establishing a _ssh tunnel_.

Open _another shell_ while keeping the previous one open. This is important!
If you close the original connection, it will shutdown your interactive session.
Now, on the new terminal connect to the cluster while passing the `-L` parameter to `ssh`
to do port forwarding from `8895` to `8080`.
Again, you can choose whatever ports you prefer --- even the same port, if you want to simplify it. 

```sh
ssh -L 8080:a240.anvil.rcac.purdue.edu:8895 -l <USERNAME> anvil.rcac.purdue.edu
```

Finally, by _keeping both terminals open_,
we can connect to the remote Jupyter server at `localhost:8080`!
The access token is the one you took note earlier.

References
==========

- [A short(ssh) guide on how to get Jupyter Notebooks up and running on the Bridges supercomputer](https://gist.github.com/mcburton/d80e4395cd82737d3677c570aa31ee40)
