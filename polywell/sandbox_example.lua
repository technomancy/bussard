-- A sample of how to embed the polywell editor in a -*- lua -*- love game.
-- In your own game loading polywell/init.lua should suffice.

local polywell = require("polywell")
local lume = require("polywell.lume")
local utf8 = require("polywell.utf8")

love.keypressed = polywell.keypressed
love.textinput = polywell.textinput
love.wheelmoved = polywell.wheelmoved
love.keyreleased = function() end

local shallow_copy = function(orig)
   local copy = {}
   for orig_key, orig_value in pairs(orig) do
      copy[orig_key] = orig_value
   end
   return copy
end

-- polywell works well with sandboxed evaluation, but if you don't need that
-- (if the user should have access to the main execution context) you can skip
-- these bits here.

local sandbox = { quit = love.event.quit,
                  editor = shallow_copy(polywell),
                  lume = shallow_copy(lume),
                  utf8 = shallow_copy(utf8),
                  utils = require("polywell.utils"),
                  table = shallow_copy(table),
                  string = shallow_copy(string),
                  math = shallow_copy(math),
                  type = type, pairs = pairs, ipairs = ipairs,
                  xpcall = xpcall, pcall = pcall,
                  debug = {traceback = debug.traceback},
                  pack = function(...) return {...} end,
                  unpack = unpack,
                  getmetatable = getmetatable,

                  -- personally I prefer serpent for this, but lume works too.
                  -- the console mode needs this for showing return values.
                  pps = lume.serialize,

                  bind = polywell.bind,
                  define_mode = polywell.define_mode,
                  print = polywell.print,
                  realprint = print,

                  graphics = love.graphics,
                  keyboard = love.keyboard,

                  -- in-game "files" are stored in this table by default.
                  fs = {
                     -- you can use this to load files off the host fs too
                     find = function(fs, path) return fs[path] end,
                  },
}

sandbox._G = sandbox

sandbox.loadstring = function(str, path)
   local chunk, err = loadstring(str, path)
   if(chunk) then setfenv(chunk, sandbox) end
   return chunk, err
end

sandbox.dofile = function(path)
   local chunk = assert(sandbox.loadstring(sandbox.fs[path], path))
   return chunk()
end

local dir

love.load = function(arg)
   dir = assert(arg[2], "Need game directory.")
   love.graphics.setFont(love.graphics.newFont("inconsolata.ttf", 16))
   love.keyboard.setKeyRepeat(true)
   love.keyboard.setTextInput(true)
   -- pre-seed the fs with all the game code
   for _,b in ipairs(love.filesystem.getDirectoryItems(dir)) do
      sandbox.fs[b] = love.filesystem.read(dir .. "/" .. b)
   end
   sandbox.dofile("init.lua")
   love.update = sandbox.update
end

love.draw = function()
   polywell.draw(nil, sandbox.draw)
end

love.quit = function()
   local function save_table(prefix)
      for name,contents in pairs(sandbox.fs) do
         if(type(contents) == "string") then
            local f = io.open(prefix .. "/" .. name, "w")
            assert(f:write(contents))
            f:close()
         elseif(type(contents) == "table") then
            save_table(contents, prefix .. "/" .. name)
         end
      end
   end
   save_table(dir)
end
