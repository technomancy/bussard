run: ; love .

VERSION=beta-2-pre

SHIP_LUA=ship/*.lua ship/editor/*.lua doc/init.lua
ENGINE_LUA=*.lua
OS_LUA=os/orb/*.lua os/lisp/*.lua
IN_OS_LUA=os/orb/resources/*
IN_SHIP_LUA=data/src/*
DEPS_LUA=globtopattern/*.lua lume/*.lua md5/*.lua os/lisp/l2l/*.lua \
	serpent/*.lua utf8/*.lua bencode/init.lua jeejah/init.lua
MISSION_LUA=data/missions/*.lua
DATA_LUA=data/*.lua data/msgs/*.lua $(MISSION_LUA)

GAME_LUA=$(SHIP_LUA) $(ENGINE_LUA) $(OS_LUA) $(IN_OS_LUA) $(IN_SHIP_LUA) $(DATA_LUA) os/lisp/resources/portal.lsp
ALL_LUA=$(GAME_LUA) $(DEPS_LUA)

PROSE=manual.md doc/*.md data/msgs/* data/motd/* data/news/* data/ships.txt data/docs/*
MEDIA=assets/* assets/fonts/*
META=readme.md LICENSE credits.md Changelog.md bussard.el

todo: ; grep -nH -e TODO $(GAME_LUA)
blockers: ; grep TODO/blocker $(GAME_LUA)
wipe_fs: ; rm -rf $(HOME)/.local/share/love/bussard/fs

# different contexts have different rules about what's OK, globals, etc
check:
	luacheck --no-color --std luajit --ignore 21/_.* \
	  --exclude-files metatable_monkey.lua --globals love lume orb pp _ \
	  -- $(ENGINE_LUA) $(SHIP_LUA) $(OS_LUA) # engine code
	luacheck --no-color --std luajit --ignore 21/_.* \
	  --globals lume utf8 pack ship pause define_mode bind utils realprint pp pps \
	            mail ssh ssh_connect portal logout ssh_send_line reply replyable \
	  -- $(IN_SHIP_LUA)
	luacheck --no-color --std luajit --ignore 21/_.* --exclude-files=*.lsp \
	  --globals lume pack orb station buy_user ship cargo_transfer refuel pps \
	            accept_mission set_prompt get_prompt buy_upgrade sell_upgrade \
	            subnet logout upgrade_help \
	  -- $(IN_OS_LUA)
	luacheck --no-color --std luajit --ignore 21/_.* --globals love lume \
	  get_prompt set_prompt \
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

releases/bussard-$(VERSION).love: $(ALL_LUA) $(PROSE) $(MEDIA) $(META) Makefile
	mkdir -p releases/
	find $(ALL_LUA) $(PROSE) $(MEDIA) $(META) -type f | LC_ALL=C sort | \
               env TZ=UTC zip -r -q -9 -X $@ -@

love: releases/bussard-$(VERSION).love

mac: love
	$(REL) $(FLAGS) -M
	mv releases/Bussard-macosx-x64.zip releases/bussard-$(VERSION)-macosx-x64.zip

windows: love
	$(REL) $(FLAGS) -W
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
