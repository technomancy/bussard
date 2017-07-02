local mission = require("mission")
local mail = require("mail")

return {
   name = "Buy battery",
   description = "Buy a battery on Merdeka Station in order to be able" ..
      " to use the portal.",

   init = function(ship, _record)
      local function battery_check()
         if(ship.upgrades["battery"]) then
            mission.record_event(ship, "buy_battery")
            mail.deliver_msg(ship, "dex19-battery.msg")
            lume.remove(ship.updaters, battery_check)
         end
      end

      table.insert(ship.updaters, battery_check)
   end,

   objectives={"buy_battery"}
}
