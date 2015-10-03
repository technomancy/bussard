run:
	love .

check:
	luacheck --no-color -g *.lua os/orb/*.lua ship/*.lua

count:
	cloc *.lua

todo:
	rgrep TODO .

clean:
	rm -rf releases/

REL=".love-release/build/love-release.sh"
FLAGS=-t bussard-$(VERSION) -a 'Phil Hagelberg' --description 'A space flight open-world exploration game, with a programmable ship and stations.' --love 0.9.1 --url https://technomancy.itch.io/bussard --version $(VERSION)

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
	scp releases/* p.hagelb.org:p/bussard/
