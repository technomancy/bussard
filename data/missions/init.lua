local lume = require("lume")
local mail = require("mail")
local mission = require("mission")
local utils = require("utils")

local help_time = 640

return {
   init = function(ship, _record)
      local function trainee_check()
         if(ship.events.trainee01) then
            mail.deliver_msg(ship, "dex19-4.msg.msg")
            mail.deliver_msg(ship, "cmec-recruit.msg")
            mission.record_event(ship, "finish_init")
         end
      end

      local function deliver_on_login()
         local merdeka = utils.find_by(ship.bodies, "name", "Merdeka Station")
         if(ship:in_range(merdeka)) then
            mail.deliver_msg(ship, "dex19-3.msg")
            lume.remove(ship.updaters, deliver_on_login)
            mission.record_event(ship, "init_login")
            table.insert(ship.updaters, trainee_check)
         elseif((utils.time(ship) - ship.events.engine_restart) > help_time
            and not ship.mail_delivered["dex19-2b"]) then
            mail.deliver_msg(ship, "dex19-2b.msg")
         end
      end

      local function engine_disabled()
         ship.engine_on = false
         if(ship.events.engine_restart) then
            mail.deliver_msg(ship, "dex19-2.msg")
            mission.accept(ship, "buy_battery")
            lume.remove(ship.updaters, engine_disabled)
            table.insert(ship.updaters, deliver_on_login)
         end
      end

      if(not ship.events.engine_restart) then
         table.insert(ship.updaters, engine_disabled)
      elseif(not ship.events.init_login) then
         table.insert(ship.updaters, deliver_on_login)
      elseif(not ship.events.trainee01) then
         table.insert(ship.updaters, trainee_check)
      end
   end,

   credits=512,
   invisible=true,
   objectives={"finish_init"},
}
