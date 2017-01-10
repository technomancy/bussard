local polywell = require("polywell")
local fs_for = require("polywell.fs")

love.keyreleased = polywell.keyreleased
love.keypressed = polywell.keypressed
love.textinput = polywell.textinput
love.wheelmoved = polywell.wheelmoved
love.mousepressed = polywell.mousepressed
love.mousereleased = polywell.mousereleased
love.mousemoved = polywell.mousemoved
love.mousefocus = polywell.mousefocus

local refresh = function()
   love.graphics.clear(love.graphics.getBackgroundColor())
   love.graphics.origin()
   polywell.draw()
   love.graphics.present()
end

love.run = function()
   love.load()
   while true do
      love.event.pump()
      local needs_refresh = false
      for name, a,b,c,d,e,f in love.event.poll() do
         if(type(love[name]) == "function") then
            love[name](a,b,c,d,e,f)
            needs_refresh = true
         elseif(name == "quit") then
            os.exit()
         end
      end
      for _,c in pairs(polywell.coroutines) do
         local ok, val = coroutine.resume(c)
         if(ok and val) then needs_refresh = true
         elseif(not ok) then print(val) end
      end
      if(needs_refresh) then refresh() end
      love.timer.sleep(0.05)
   end
end

love.load = function()
   local multiprint = function(x) print(x) polywell.print(x) end
   love.graphics.setFont(love.graphics.newFont("inconsolata.ttf", 14))
   love.keyboard.setTextInput(true)
   love.keyboard.setKeyRepeat(true)

   local init_file = love.filesystem.getUserDirectory() .. "/.polywell/init.lua"
   local ok, err = pcall(dofile, init_file)
   if(not ok) then
      multiprint(err)
      multiprint("Using default config.")
      local chunk = assert(love.filesystem.load("polywell/config/init.lua"))
      chunk("polywell_app")
   end
   polywell.fs = polywell.fs or fs_for(os.getenv("PWD") or ".")
   polywell.open(polywell.fs, arg[2] or "*console*")
end
