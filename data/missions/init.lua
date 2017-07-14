local mail = require("mail")
local mission = require("mission")
local utils = require("utils")

local coro = coroutine.create(function(ship)
      while(not ship.events.engine_restart) do
         ship.engine_on = false
         coroutine.yield()
      end

      mail.deliver_msg(ship, "dex19-2.msg")
      mission.accept(ship, "buy_battery")

      while(not ship.events.init_login) do
         local merdeka = utils.find_by(ship.bodies, "name", "Merdeka Station")
         if(ship:in_range(merdeka)) then
            mail.deliver_msg(ship, "dex19-3.msg")
            mission.record_event(ship, "init_login")
         elseif(utils.time(ship) - ship.events.engine_restart > 640) then
            mail.deliver_msg(ship, "dex19-2b.msg")
         end
         coroutine.yield()
      end
end)

return {
   init = function(ship) table.insert(ship.updaters, coro) end,
   on_success = function(ship)
      mail.deliver_msg(ship, "dex19-4.msg")
      mail.deliver_msg(ship, "cmec-recruit.msg")
   end,

   credits=512,
   invisible=true,
   objectives={"trainee01"},
}
