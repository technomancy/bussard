local ship = require("ship")
local lume = require("lume")
local editor = ship.api.editor

local seed = tonumber(os.getenv("BUSSARD_FUZZ_SEED") or os.time())
print("seeding with", seed)
math.randomseed(seed)

local mock = {}
setmetatable(mock, {__index= function() return function() return mock end end})
love.graphics = mock

local function vals(t)
   local rtn = {}
   for k,v in pairs(t) do rtn[#rtn + 1] = v end
   return rtn
end

local find = function(mode, command)
   for mn,m in pairs({main=mode.map, ctrl=mode.ctrl,
                      alt=mode.alt, ctrl_alt=mode["ctrl-alt"]}) do
      for k,v in pairs(m) do
         if(v == command) then return mn .. "-" .. k end
      end
   end
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
      love.filesystem.append("seeds", tostring(seed) .. "\n")
      os.exit(1)
   end
end

local function fuzz(n)
   for i=1,n do
      if(#editor.buffer_names() == 1) then
         editor.open(ship.api, "newfile")
      else
         editor.change_buffer(lume.randomchoice(editor.buffer_names()))
      end

      local mode = editor.mode()
      local commands = lume.concat(vals(mode.map), vals(mode.ctrl),
                                   vals(mode.alt), vals(mode["ctrl-alt"]))
      local command = lume.randomchoice(commands)

      print("running " .. find(mode, command) .. " in mode " .. mode.name)
      try(lume.fn(editor.wrap, command))

      if(love.math.random(5) == 1) then
         local input = ""
         for _ = 1,love.math.random(10) do
            input = input .. string.char(love.math.random(127-32) + 32)
            if(love.math.random(5) == 1) then input = input .. " " end
         end
         try(lume.fn(editor.handle_textinput, input))
      end
   end
end

editor.open(ship.api, "newfile")

fuzz(tonumber(os.getenv("BUSSARD_FUZZ_COUNT") or 256))
os.exit(0)
