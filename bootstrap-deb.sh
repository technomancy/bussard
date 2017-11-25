#!/bin/bash

# This script is used in CI but could also be helpful for getting your own
# dev machine set up.

apt-get update -qq && apt-get install -qq lua-check cloc make wget
wget "https://bitbucket.org/rude/love/downloads/liblove0_0.10.2ppa1_amd64.deb"
wget "https://bitbucket.org/rude/love/downloads/love_0.10.2ppa1_amd64.deb"
dpgk -i liblove0_0.10.2ppa1_amd64.deb
dpkg -i love_0.10.2ppa1_amd64.deb
apt-get -f install
