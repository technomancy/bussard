#!/bin/bash

# This script is used in CI but could also be helpful for getting your own
# dev machine set up.
export TERM=dumb
apt-get update -qq && apt-get install -qq cloc make wget luajit

# In CI we use an image which has love already, because the debs below segfault
# when run in docker for some reason.
if [ "$(which love 2> /dev/null)" = "" ]; then
    wget "https://bitbucket.org/rude/love/downloads/liblove0_0.10.2ppa1_amd64.deb"
    wget "https://bitbucket.org/rude/love/downloads/love_0.10.2ppa1_amd64.deb"
    dpkg -i liblove0_0.10.2ppa1_amd64.deb
    dpkg -i love_0.10.2ppa1_amd64.deb
    apt-get -f -y install
fi

# If you're on a newer Debian you can use lua-check from apt-get, but the one
# in stable is too old.
wget https://github.com/mpeterv/luacheck/archive/0.21.2.tar.gz
tar xzf 0.21.2.tar.gz && cd luacheck-0.21.2 && luajit install.lua /usr/local
