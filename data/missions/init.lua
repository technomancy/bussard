local lume = require("lume")
local mail = require("mail")
local client = require("os.client")

return {
   init = function(ship, _record)
      local function deliver_on_login()
         -- TODO: this isn't working
         if(client.is_connected(ship, "Merdeka Station")) then
            mail.deliver_msg(ship, "dex19-3.msg")
            lume.remove(ship.updaters, deliver_on_login)
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
      table.insert(ship.updaters, engine_disabled)
   end,
   invisible=true
}
