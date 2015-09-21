run:
	love .

check:
	luacheck --no-color -g *.lua os/orb/*.lua

count:
	cloc *.lua

todo:
	rgrep TODO .
