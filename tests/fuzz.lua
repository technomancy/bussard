local ship = require("ship")
local lume = require("lume")
local editor = ship.api.editor

-- ctrl-` is bound to load host.config for dev convenience
ship.api.host = {config="return nil"}

local mock = {}
setmetatable(mock, {__index= function() return function() return mock end end})
love.graphics = mock

-- need to display the seed so we can replay problematic sequences
local seed = tonumber(os.getenv("BUSSARD_FUZZ_SEED") or os.time())
print("seeding with", seed)
math.randomseed(seed)
love.math.setRandomSeed(seed)

local function vals(t)
   local rtn = {}
   for k,v in pairs(t) do rtn[#rtn + 1] = v end
   return rtn
end

local try = function(f)
   local traceback, err
   local ok, ret = xpcall(f, function(e)
                             traceback, err = debug.traceback(), e
   end)
   if(not ok) then
      print(err)
      print(traceback)
      editor.debug()
      print("BUSSARD_FUZZ_SEED=" .. seed .. " make fuzz")
      love.filesystem.append("seeds", tostring(seed) .. "\n")
      os.exit(1)
   end
end

local random_text = function()
   local input = ""
   for _ = 1,love.math.random(10) do
      input = input .. string.char(love.math.random(127-32) + 32)
      if(love.math.random(5) == 1) then input = input .. " " end
   end
   return input
end

local commands_by_mode = {}
local binding_for = {}
local function commands_for(mode)
   if(commands_by_mode[mode]) then return commands_by_mode[mode] end
   -- smush together all the different sub-maps (ctrl, alt, ctrl-alt)
   local commands = lume.merge(mode.map, mode.ctrl,
                               mode.alt, mode["ctrl-alt"])
   local parent = mode.parent and editor.debug("modes")[mode.parent]
   commands_by_mode[mode] = lume.concat(vals(commands),
                                        parent and commands_for(parent))
   for key,command in pairs(commands) do
      if(lume.find(mode.ctrl, command)) then key = "ctrl-" .. key
      elseif(lume.find(mode.alt, command)) then key = "alt-" .. key
      elseif(lume.find(mode["ctrl-alt"], command)) then key = "ctrl-alt" .. key
      end
      binding_for[command] = key
   end
   for i,c in lume.ripairs(commands_by_mode[mode]) do
      if(type(c) == "table") then -- prefix maps
         lume.extend(commands_by_mode[mode],
                     table.remove(commands_by_mode[mode], i))
      end
   end
   return commands_by_mode[mode]
end

local function fuzz(n)
   for i=1,n do
      if(#editor.buffer_names() == 1 and
         editor.current_buffer_path ~= "minibuffer") then
         print("opening newfile")
         editor.open(ship.api, "newfile")
      end

      local mode = editor.debug("modes")[editor.current_mode_name()]
      local command = assert(lume.randomchoice(commands_for(mode)),
                             "no command in " .. mode.name)

      print("run " .. binding_for[command] .. " in mode " .. mode.name)
      try(lume.fn(editor.debug("wrap"), command))

      -- the minibuffer doesn't activate till you run this to work around
      -- a race condition in keyboard input.
      if(love.keyreleased) then love.keyreleased() end

      -- sometimes we should try inserting some text too
      if(love.math.random(5) == 1) then
         try(lume.fn(editor.textinput, random_text()))
      end
   end
end

editor.open(ship.api, "newfile")
-- TODO: load map code so we can fuzz that too

fuzz(tonumber(os.getenv("BUSSARD_FUZZ_COUNT") or 256))
os.exit(0)
