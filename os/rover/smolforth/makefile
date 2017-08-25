CODE=init.lua
DEPS=lib/*lua
LUA=luajit

run:
	$(LUA) init.lua

luacheck:
	luacheck --no-color --std luajit --ignore 21/_.* \
	  --globals pp _ -- $(CODE)

test:
	$(LUA) test.lua

count: ; cloc --force-lang=lua $(CODE)

count_all: ; cloc --force-lang=lua $(CODE) $(DEPS)

ci: luacheck test
