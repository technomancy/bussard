run: ; love .

VERSION=beta-3-pre

SHIP_LUA=ship/*.lua doc/init.lua os/client.lua
ENGINE_LUA=*.lua
OS_LUA=os/orb/*.lua os/lisp/*.lua os/rover/*.lua os/server.lua
IN_OS_LUA=os/orb/resources/*
IN_SHIP_LUA=data/src/*
DEPS_LUA=globtopattern/*.lua lume/*.lua md5/*.lua os/lisp/l2l/*.lua \
	serpent/*.lua bencode/init.lua jeejah/init.lua
MISSION_LUA=data/missions/*.lua
DATA_LUA=data/*.lua data/msgs/*.lua $(MISSION_LUA)
POLYWELL=polywell/*.lua polywell/lume/init.lua polywell/utf8/init.lua

GAME_LUA=$(SHIP_LUA) $(ENGINE_LUA) $(OS_LUA) $(IN_OS_LUA) $(IN_SHIP_LUA) $(DATA_LUA) os/lisp/resources/portal.lsp
ALL_LUA=$(GAME_LUA) $(DEPS_LUA)

PROSE=manual.md doc/*.md data/msgs/* data/motd/* data/subnet/* data/ships.txt data/docs/*
MEDIA=assets/* assets/fonts/*
META=readme.md LICENSE credits.md Changelog.md

todo: ; grep -nH -e TODO $(GAME_LUA)
blockers: ; grep TODO/blocker $(GAME_LUA)
wipe_fs: ; rm -rf $(HOME)/.local/share/love/bussard/fs

# different contexts have different rules about what's OK, globals, etc
check:
	luacheck --no-color --std luajit --ignore 21/_.* \
	  --exclude-files metatable_monkey.lua --globals love lume orb pp _ \
	  -- $(ENGINE_LUA) $(SHIP_LUA) $(OS_LUA) # engine code
	luacheck --no-color --std luajit --ignore 21/_.* \
	  --globals lume utf8 pack ship define_mode bind utils realprint pp pps \
	            mail ssh ssh_connect portal logout ssh_send_line reply \
	            universe graphics editor toggle_fps replyable flight_draw \
	            ssh_get_connection rover_operate tetris \
	  -- $(IN_SHIP_LUA)
	luacheck --no-color --std luajit --ignore 21/_.* --exclude-files=*.lsp \
	  --globals io lume orb station buy_user ship cargo_transfer pps \
	            accept_mission get_prompt set_prompt buy_upgrade sell_upgrade \
	            list_upgrades subnet logout upgrade_help port loan \
	            cargo_prices cargo_amounts cargo_hold refuel fuel_price \
	  -- $(IN_OS_LUA)
	luacheck --no-color --std luajit --ignore 21/_.* --globals love lume term \
	  -- $(DATA_LUA)

test: ; love . --test

fuzz: ; love . --fuzz

ci: check test fuzz

count: ; cloc --force-lang=lua $(GAME_LUA)

count_engine: ; cloc $(ENGINE_LUA) $(SHIP_LUA)

count_data: ; cloc --force-lang=lua $(IN_SHIP_LUA) $(IN_OS_LUA) $(OS_LUA) \
	  $(DATA_LUA)

count_deps: ; cloc $(DEPS_LUA)

count_all: ; cloc $(ALL_LUA)

count_prose: ; find $(PROSE) -type f -print0 | xargs -0 wc -l

clean: ; rm -rf releases/

REL=".love-release/build/love-release.sh"
FLAGS=-a 'Phil Hagelberg' -x spoilers \
	--description 'A space flight programming adventure game.' \
	--love 0.10.1 --url https://technomancy.itch.io/bussard --version $(VERSION)

releases/bussard-$(VERSION).love: $(ALL_LUA) $(PROSE) $(MEDIA) $(META) $(POLYWELL) Makefile
	mkdir -p releases/
	find $(ALL_LUA) $(PROSE) $(MEDIA) $(META) $(POLYWELL) -type f | LC_ALL=C sort | \
               env TZ=UTC zip -r -q -9 -X $@ -@

love: releases/bussard-$(VERSION).love

mac: love
	$(REL) $(FLAGS) -M
	mv releases/Bussard-macosx-x64.zip releases/bussard-$(VERSION)-macosx-x64.zip

windows: love
	$(REL) $(FLAGS) -W -W32
	mv releases/Bussard-win32.zip releases/bussard-$(VERSION)-windows.zip

deb: love
	dpkg-buildpackage -us -uc -b

# don't use this until https://github.com/itchio/butler/issues/51 is fixed!
pushlove: love
	butler push releases/bussard-$(VERSION).love technomancy/bussard:love

pushmac: mac
	butler push releases/bussard-$(VERSION)-macosx-x64.zip technomancy/bussard:mac

pushwindows: windows
	butler push releases/bussard-$(VERSION)-windows.zip technomancy/bussard:windows

push: pushmac pushwindows
	echo "Upload releases/bussard-$(VERSION).love manually in the browser for now."
