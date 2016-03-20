run:
	love .

check:
	luacheck --no-color --std luajit --ignore 21/_.* \
	  --exclude-files metatable_monkey.lua headless.lua ship/ssh.lua \
	  --globals love lume orb _ \
	  -- *.lua ship/*.lua os/lisp/*.lua
	luacheck --no-color --std luajit --ignore 21/_.* --no-unused \
	  --globals lume pack ship pause define_mode bind utils \
	            ssh ssh_connect logout ssh_send_line \
	  -- data/src/*
	luacheck --no-color --std luajit --ignore 21/_.* \
	  --globals lume pack orb station buy_user ship cargo_transfer refuel \
	            accept_mission set_prompt buy_upgrade sell_upgrade upgrade_help \
	 -- os/orb/resources/*

count:
	cloc *.lua ship/*.lua

countall:
	cloc *.lua ship/*.lua os/orb/*.lua os/lisp/*.lua os/lisp/resources/* os/orb/resources/*

todo:
	rgrep TODO .

clean:
	rm -rf releases/

REL=".love-release/build/love-release.sh"
FLAGS=-a 'Phil Hagelberg' -x spoilers --description 'A space flight open-world exploration game, with a programmable ship and stations.' --love 0.9.1 --url https://technomancy.itch.io/bussard --version $(VERSION)

love:
	$(REL) $(FLAGS) -L

mac:
	$(REL) $(FLAGS) -M

windows:
	$(REL) $(FLAGS) -W

release: love mac windows

sign:
	gpg -ab releases/bussard-*

upload: release sign
	rsync -r releases/ p.hagelb.org:p/bussard/

systems:
	lua -lheadless data/systems.lua
