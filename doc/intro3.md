# Stations

Once you've stabilized your orbit a bit, log back into the
station. There are a few things you can do here. This will show you
the full list of commands you can run:

    ls /bin

Most commands explain how they work if you run them with "--help".
The most important to you right now is the upgrade command. You're
going to want a mining laser for your ship, so run this next:

    upgrade buy laser

Before you go, you can read some of the newsgroup postings. To see
which groups are available, run this:

    ls /usr/news

Let's take a look at the "soc.tana" group, which has discussion about
the Tana Protectorate, the government that controls the system you're
currently in.

    cd /usr/news/soc.tana

Running "ls" will list all the threads in this group. In order to read
one of them, run this:

    cat relations-with-sol

OK, that's enough at the station for now. You'll want to come back
later and try the "refuel" and "cargo" commands, but for now logout
and continue with:

    man("intro4")
