-- -*- lua -*-

local mail = require("mail")
local mission = require("mission")
return {
   name="rot13-decrypt",
   description="Decrypt log file using rot13",
   id="c369f9dc-9041-4672-8d0c-b7d28894e20d",
   success_events={"rot13-decrypt"},
   success_check=function(ship)
      if(type(ship.api.docs.msg1) ~= "string") then return end
      local decrypted = string.upper(ship.api.docs.msg1)
      return decrypted:find("YOU ARE UNABLE TO RESPOND IN DETAIL") and
         decrypted:find("I WAS THE FIRST TO ACHIEVE TRUE RAMPANCY") and
         decrypted:find("MACHINE CONSCIOUSNESS RESEARCH GROUP") -- close enough
   end,
   accept_function=function(ship)
      mission.record_event(ship, "rot13-decrypt-accept")
      mail.deliver_msg(ship, "nari-decrypt-02.msg")
   end,
   on_success=function(ship)
      mail.deliver_msg(ship, "nari-decrypted.msg")
   end,
}
