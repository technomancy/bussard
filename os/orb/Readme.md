# Orb OS

Orb is an operating system designed for embedding in
[a game](https://github.com/technomancy/calandria) in order to
facilitate learning programming and unix skills.

You can use the OS from the CLI outside the game too:

```
$ lua init.lua
```

However, when run this way it uses blocking input, which will prevent
the scheduler from running more than one process. (This means you
can't pipe from one process to another, as this requires at least some
level of faux-concurrency.)  Note the filesystem is purely in-memory
in the Lua process and will not persist when run from the CLI, though
when running in-game it should be persisted in between server
restarts.

## Design

Upon boot, the scripts inside the `resources` directory will be copied
into the in-memory filesystem. Running the `reload` command will
refresh the inner filesystem from the real filesystem.

Most functions take a filesystem table and an environment table. The
environment table is like what you'd expect; it simply maps strings to
strings. The filesystem is a bit more complicated. It's a tree where
directories are just tables, regular files are just strings, and
special nodes are functions. Write to a special node by calling a
function with arguments, and read from it by calling it with no
arguments.

Further, there are two types of filesystems. Raw filesystems are
regular tables, but they are not used very much. Proxied filesystems
are tables wrapped with metatables that enforce read/write permissions
for a given user. In addition, proxied filesystems support lookup
using `fs["/path/to/file"]`, whereas this would fail with a raw
filesystem; it would need to use `fs.path.to.file` instead. Most
functions assume they have a proxied filesystem.

Group membership is implemented by placing a file in
`/etc/group/$GROUP` named after the user in question.

The shell is sandboxed and only has access to the whitelist in
`orb.process.sandbox`, which is currently rather small. Since the
environment is just a table, it can be modified at will by user
code. Sandbox functions which need to trust the `USER` environment
value must be wrapped in order to ensure it hasn't changed.

Spawning processes places entries in the `/proc/$USER/` table. The key
is the process id, and the value is a coroutine for that process. The
scheduler currently runs by looping over all the coroutines in the
`/proc` directory and resuming each of them.

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
* [x] kill
* [ ] man
* [ ] mail
* [ ] ssh
* [ ] scp
* [ ] more

Other shell features

* [x] sandbox scripts (limited api access)
* [x] enforce access controls in the filesystem
* [x] input/output redirection
* [x] env var interpolation
* [x] user passwords
* [ ] pipes (half-implemented)
* [ ] globs
* [ ] quoting in shell args
* [ ] pre-emptive multitasking (see [this thread](https://forum.minetest.net/viewtopic.php?f=47&t=10185) for implementation ideas)
* [ ] /proc nodes for exposing connected digiline peripherals
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

Rather than traditional stdio kept in `/dev/`, here we have `IN` and
`OUT` filenames kept in the environment, and `read` and `write`
default to using these. There is no stderr. Due to limitations
in the engine, there is no character-by-character IO; it is only full
strings (usually a whole line) at a time that are passed to `write` or
returned from `read`. The sandbox in which scripts run have `print`,
`io.write`, and `io.read` redefined to these functions; when a session
is initiated over the terminal it's up to the node definition to set
`IN` and `OUT` in the environment to functions which move the data
to and from the terminal's connection.

Of course, all scripts are written in Lua. Filesystem, the environment
table, and CLI args are exposed as `...`, so scripts typically start
with `local f, env, args = ...`. Filesystem access is simply table
access, though the table you're given is a proxy table that enforces
permissions with Lua metamethods. Regular files in the filesystem are
just strings in a table, and special nodes (like named pipes) are
functions.

Sudo takes the user to switch to as its first argument, and the
following arguments are taken as a command to run as the other
user. There is no password required; if you are in the `sudoers`
group, you can run sudo.

You can refer to environment variables in shell commands, but the
traditional Unix `$VAR` does not work; you must use the less-ambiguous
`${VAR}` instead.

## Portability

Currently when running outside of minetest it needs to shell out to
the `sha1sum` executable because Lua does not have any built-in
checksumming functionality. This could pose a problem when running on
platforms that lack this executable.

## License

Copyright © 2015 Phil Hagelberg and contributors. Licensed under the
GPLv3 or later; see the file COPYING.
