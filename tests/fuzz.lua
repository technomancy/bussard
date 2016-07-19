local ship = require("ship")
local lume = require("lume")
local editor = ship.api.editor

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
         if(v == command) then return mn,k end
      end
   end
end

local function fuzz(n)
   if(n <= 0) then return love.event.quit() end
   local mode = editor.mode()
   local commands = lume.concat(vals(mode.map), vals(mode.ctrl),
                                vals(mode.alt), vals(mode["ctrl-alt"]))
   local command = lume.randomchoice(commands)
   print("running", find(mode, command))
   command()
   if(editor.mode() ~= mode) then
      print("Changed mode:", editor.mode().name)
   end
   local input = ""
   for _ = 1,love.math.random(10) do
      input = input .. string.char(love.math.random(127-32) + 32)
      if(love.math.random(5) == 1) then input = input .. " " end
   end
   print("inserting", input)
   editor.handle_textinput(input)

   fuzz(n-1)
end

editor.open(ship.api, "newfile")

fuzz(tonumber(os.getenv("BUSSARD_FUZZ_COUNT") or 128))
