run: ; love .

VERSION=beta-1

SHIP_LUA=ship/*.lua
ENGINE_LUA=*.lua
OS_LUA=os/orb/*.lua os/lisp/*.lua
IN_OS_LUA=os/orb/resources/*
IN_SHIP_LUA=data/src/*
DEPS_LUA=globtopattern/*.lua lume/*.lua md5/*.lua os/lisp/l2l/*.lua
MISSION_LUA=data/missions/*.lua
DATA_LUA=data/*.lua

GAME_LUA=$(SHIP_LUA) $(ENGINE_LUA) $(OS_LUA) $(IN_OS_LUA) $(IN_SHIP_LUA)
ALL_LUA=$(GAME_LUA) $(DEPS_LUA)

PROSE_DIRS=doc data/msgs data/motd data/news

todo: ; grep -nH -e TODO $(GAME_LUA)
blockers: ; grep TODO/blocker $(GAME_LUA)

SAVE_DIR=${HOME}/.local/share/love/bussard

wipe: ; rm -rf ${SAVE_DIR}
backup: ; rm -rf ${SAVE_DIR}.bak; mv ${SAVE_DIR} ${SAVE_DIR}.bak
restore: wipe ; cp -r ${SAVE_DIR}.bak ${SAVE_DIR}

check:
	luacheck --no-color --std luajit --ignore 21/_.* \
	  --exclude-files metatable_monkey.lua --globals love lume orb _ \
	  -- $(ENGINE_LUA) $(SHIP_LUA) $(OS_LUA)
	luacheck --no-color --std luajit --ignore 21/_.* --no-unused \
	  --globals lume utf8 pack ship pause define_mode bind utils realprint pp pps \
	            mail ssh ssh_connect portal logout ssh_send_line reply replyable \
	  -- $(IN_SHIP_LUA)
	luacheck --no-color --std luajit --ignore 21/_.* \
	  --globals lume pack orb station buy_user ship cargo_transfer refuel \
	            accept_mission set_prompt buy_upgrade sell_upgrade upgrade_help \
	  -- $(IN_OS_LUA)

count: ; cloc --force-lang=lua $(GAME_LUA)

count_engine: ; cloc $(ENGINE_LUA) $(SHIP_LUA)

count_data: ; cloc --force-lang=lua $(IN_SHIP_LUA) $(IN_OS_LUA) $(OS_LUA) \
	  $(MISSION_LUA) $(DATA_LUA)

count_deps: ; cloc $(DEPS_LUA)

count_prose: ; find $(PROSE_DIRS) -type f -print0 | xargs -0 wc -l

clean: ; rm -rf releases/

REL=".love-release/build/love-release.sh"
FLAGS=-a 'Phil Hagelberg' -x spoilers --description 'A space flight programming adventure game.' --love 0.9.1 --url https://technomancy.itch.io/bussard --version $(VERSION)

love: $(ALL_LUA)
	$(REL) $(FLAGS) -L
	cp releases/Bussard.love releases/bussard-$(VERSION).love

mac: love
	$(REL) $(FLAGS) -M
	mv releases/Bussard-macosx-x64.zip releases/bussard-$(VERSION)-macosx-x64.zip

windows: love
	$(REL) $(FLAGS) -W
	mv releases/Bussard-win32.zip releases/bussard-$(VERSION)-windows.zip

# don't use this until https://github.com/itchio/butler/issues/51 is fixed!
pushlove: love
	butler push releases/bussard-$(VERISON)-love technomancy/bussard:love

pushmac: mac
	butler push releases/bussard-$(VERISON)-macosx-x64.zip technomancy/bussard:mac

pushwindows: windows
	butler push releases/bussard-$(VERISON)-windows.zip technomancy/bussard:windows

push: pushlove pushmac pushwindows
