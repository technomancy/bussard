local mission = require("mission")
local mail = require("mail")

local coro = coroutine.create(function(ship)
      while(not ship.upgrades["battery"]) do coroutine.yield() end
      mission.record_event(ship, "buy_battery")
      mail.deliver_msg(ship, "dex19-battery.msg")
end)

return {
   init = function(ship) table.insert(ship.updaters, coro) end,
   name = "Buy battery",
   description = "Buy a battery on Merdeka Station in order to be able" ..
      " to use the portal.",
   objectives={"buy_battery"}
}
