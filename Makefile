run:
	love .

check:
	luacheck --no-color -g *.lua os/orb/*.lua os/orb/resources/*.lua

count:
	cloc *.lua

todo:
	rgrep TODO .
