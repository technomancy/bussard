# Orb OS

Orb is an operating system designed for embedding in
[a game](https://github.com/technomancy/calandria) in order to
facilitate learning programming and unix skills.

## Design

Most functions take an environment table and an args list. The
environment table is like what you'd expect; it simply maps strings to
strings.

Filesystem operations are written to `fs/$HOSTNAME` inside the LÖVE save
directory. They go through `orb.fs.read/write` to check permissions, and
in the future handle pipes by using channels to send lines between threads.
Upon boot, the scripts inside the `resources` directory will be copied into
the filesystem; these are considered the userspace; the kernel is all the 
top-level `*.lua` files in this directory.

Group membership is implemented by placing a file in
`/etc/group/$GROUP` named after the user in question.

The shell is sandboxed and only has access to the whitelist in
`orb.sandbox`, which is currently rather small. Since the
environment is just a table, it can be modified at will by user
code. Sandbox functions which need to trust the `USER` environment
value must be wrapped in order to ensure it hasn't changed.

Each session has its own LÖVE thread, which is completely isolated from others
except through channels, which are mostly used for stdio. They are also used
for rpc when orb scripts need to make calls to functions in another thread. On
login, wrapper functions for each rpc function are inserted into the local
sandbox which push operations onto a channel and wait for a response.

## Executables

* [x] ls
* [x] cat
* [x] mkdir
* [x] env
* [x] cp
* [x] mv
* [x] rm
* [x] echo
* [x] smash (bash-like)
* [x] chmod
* [x] chown
* [x] chgrp
* [x] ps
* [x] grep
* [x] sudo
* [x] passwd
* [ ] mkfifo
* [ ] kill
* [ ] man
* [ ] mail
* [ ] ssh
* [ ] scp
* [ ] more

Other shell features

* [x] sandbox scripts (limited api access)
* [x] enforce access controls in the filesystem
* [ ] input/output redirection
* [x] env var interpolation
* [x] user passwords
* [ ] pipes (half-implemented)
* [ ] globs
* [ ] quoting in shell args
* [ ] more of the built-in scripts should take multiple target arguments

## Differences from Unix

The OS is an attempt at being unix-like; however, it varies in several
ways. Some of them are due to conceptual simplification; some are in
order to have an easier time implementing it given the target
platform, and some are due to oversight/mistakes or unfinished features.

The biggest difference is that of permissions. In this system,
permissions only belong to directories, and files are simply subject
to the permissions of the directory containing them. In addition, the
[octal permissions](https://en.wikipedia.org/wiki/File_system_permissions#Notation_of_traditional_Unix_permissions)
of unix are collapsed into a single `group_write` bit. It's assumed
that the directory's owner always has full read-write access and that
members of the group always have read access. The `chown` and `chgrp`
commands work similarly as to unix, but `chmod` simply takes a `+` or
`-` argument to enable or disable group write. Group membership is
indicated simply by an entry in the `/etc/groups/$GROUP` directory
named after the username.

Sudo takes the user to switch to as its first argument, and the
following arguments are taken as a command to run as the other
user. There is no password required; if you are in the `sudoers`
group, you can run sudo.

You can refer to environment variables in shell commands, but the
traditional Unix `$VAR` does not work; you must use the less-ambiguous
`${VAR}` instead.

## License

Copyright © 2015-2017 Phil Hagelberg and contributors. Licensed under the
GPLv3 or later; see the file COPYING.
