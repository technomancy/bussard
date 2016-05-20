# Quickstart, continued (communications)

Your ship is also equipped with a versatile communications system.
First try opening the mail client by pressing ctrl-m. This will show
an overview of the folders, each one with a count of unread/total
messages. Pressing enter will open a folder or, once you have opened a
folder, will open a message.

The mail client shows both messages sent directly to you as well as
messages from local mailing lists, though you will only be notified in
your HUD about messages for you. Some messages you can reply to with
alt-return; this is most often used for accepting missions which are
posted to various mailing lists. Pressing alt-a will archive a message.

Your ship's onboard computer also contains an SSH client, which allows
you to connect with the port computers at various stations and
planets. You can log in by targeting a world with a port computer and
flying within range (your targeting indicator will turn bright green
when in range) then pressing ctrl-s, or by running ssh() from the
console.

The commands you can run when logged in vary from port to port, but
usually they will at least sell ship upgrades with the "upgrade"
command and trade cargo with the "cargo" command. Use "ls /bin" to
list all commands. Most commands take a "--help" argument.

In order to activate interstellar portals, you must fly close enough
for the targeting indicator to turn dark blue, then press ctrl-p. To
successfully travel through the portal, you must stay within range
until the process is complete.

Try out these things, then when you've finished run:

    man("quickstart3")
