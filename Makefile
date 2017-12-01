run: ; love . --cheat

VERSION=beta-4-pre

SHIP_LUA=ship/*.lua doc/init.lua os/client.lua
ENGINE_LUA=*.lua
OS_LUA=os/orb/*.lua os/lisp/*.lua os/rover/*.lua os/server.lua
IN_OS_LUA=os/orb/resources/* data/host_src/*.lua data/maps/*.lua
IN_SHIP_LUA=data/src/*
DEPS_LUA=globtopattern/*.lua lume/*.lua os/lisp/l2l/*.lua \
	serpent/*.lua bencode/init.lua jeejah/init.lua os/rover/smolforth/init.lua
MISSION_LUA=data/missions/*.lua
DATA_LUA=data/*.lua data/msgs/*.lua $(MISSION_LUA)
POLYWELL=polywell/*.lua polywell/lume/init.lua polywell/utf8/init.lua
SHADERS=*.glsl

GAME_LUA=$(SHIP_LUA) $(ENGINE_LUA) $(OS_LUA) $(IN_OS_LUA) $(IN_SHIP_LUA) \
	$(DATA_LUA) os/lisp/resources/portal.lsp
ALL_LUA=$(GAME_LUA) $(DEPS_LUA)

PROSE=manual.md doc/*.md data/msgs/* data/motd/* data/subnet/* data/ships.txt data/docs/*
MEDIA=assets/* assets/fonts/*
META=readme.md LICENSE credits.md Changelog.md

todo: ; grep -nH -e TODO $(GAME_LUA) || true
blockers: ; grep TODO/blocker $(GAME_LUA) || true
wipe: ; love . --wipe
wipe_fs: ; rm -rf $(HOME)/.local/share/love/bussard/fs

# rules for each section are defined in .luacheckrc
luacheck:
	luacheck --std luajit+engine $(ENGINE_LUA) $(SHIP_LUA) $(OS_LUA)
	luacheck --std luajit+inship $(IN_SHIP_LUA)
	luacheck --std luajit+inos   $(IN_OS_LUA)
	luacheck --std luajit+data   $(DATA_LUA)

test: ; love . --test

fuzz: ; love . --fuzz

ci: luacheck test fuzz count_all

count: ; cloc --force-lang=lua $(GAME_LUA)

count_engine: ; cloc $(ENGINE_LUA) $(SHIP_LUA) $(OS_LUA)

count_data: ; cloc --force-lang=lua $(IN_SHIP_LUA) $(IN_OS_LUA) $(OS_LUA) \
	  $(DATA_LUA)

count_deps: ; cloc $(DEPS_LUA)

count_all: ; cloc $(ALL_LUA)

count_prose: ; find $(PROSE) -type f -print0 | xargs -0 wc -l

clean: ; rm -rf releases/ bussard.love

REL="$(PWD)/love-release.sh"
FLAGS=-a 'Phil Hagelberg' -x spoilers -x savedir \
	--description 'A space flight programming adventure game.' \
	--love 0.10.2 --url https://technomancy.itch.io/bussard --version $(VERSION)

releases/bussard-$(VERSION).love: $(ALL_LUA) $(SHADERS) $(PROSE) $(MEDIA) $(META) $(POLYWELL) Makefile
	mkdir -p releases/
	find $(ALL_LUA) $(SHADERS) $(PROSE) $(MEDIA) $(META) $(POLYWELL) -type f | LC_ALL=C sort | \
               env TZ=UTC zip -r -q -9 -X $@ -@

love: releases/bussard-$(VERSION).love

mac: love
	$(REL) $(FLAGS) --lovefile releases/bussard-$(VERSION).love -M
	mv releases/Bussard-macosx-x64.zip releases/bussard-$(VERSION)-macosx-x64.zip

windows: love
	$(REL) $(FLAGS) --lovefile releases/bussard-$(VERSION).love -W32
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

# upload prerelease versions to scratch site instead of itch.io
preupload: love mac windows
	scp releases/bussard-$(VERSION)-* p.hagelb.org:p/
