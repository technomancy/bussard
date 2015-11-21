run:
	love .

check:
	luacheck --no-color --std luajit --exclude-files metatable_monkey.lua headless.lua \
	  --new-globals love lume orb _ -- *.lua os/orb/*.lua ship/*.lua os/lisp/*.lua

count:
	cloc *.lua os/orb/*.lua os/lisp/*.lua os/lisp/resources/* ship/*.lua os/orb/resources/*

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
