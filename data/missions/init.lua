local lume = require("lume")
local mail = require("mail")
local mission = require("mission")
local client = require("os.client")
local utils = require("utils")

local help_time = 300

return {
   init = function(ship, _record)
      local function deliver_on_login()
         if(client.is_connected(ship, "Merdeka Station")) then
            mail.deliver_msg(ship, "dex19-3.msg")
            lume.remove(ship.updaters, deliver_on_login)
            mission.record_event(ship, "init_login")
         elseif((utils.time(ship) - ship.events.engine_restart) > help_time
            and not ship.mail_delivered["dex19-2b"]) then
            mail.deliver_msg(ship, "dex19-2b.msg")
         end
      end

      local function engine_disabled()
         ship.engine_on = false
         if(ship.events.engine_restart) then
            mail.deliver_msg(ship, "dex19-2.msg")
            lume.remove(ship.updaters, engine_disabled)
            table.insert(ship.updaters, deliver_on_login)
         end
      end

      if(not ship.events.engine_restart) then
         table.insert(ship.updaters, engine_disabled)
      elseif(not ship.events.init_login) then
         table.insert(ship.updaters, deliver_on_login)
      end
   end,
   invisible=true,
   objectives={"finish_init"},
}
