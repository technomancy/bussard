run:
	love .

check:
	luacheck --no-color -g *.lua

count:
	cloc *.lua
