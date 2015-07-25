check:
	luacheck --no-color -g *.lua

run:
	love .

count:
	cloc *.lua
