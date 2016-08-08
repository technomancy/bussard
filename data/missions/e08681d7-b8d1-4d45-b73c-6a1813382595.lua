local mail = require("mail")
local mission = require("mission")

return {
   name="find-dorath",
   description="Track down Elwin Dorath",
   id="e08681d7-b8d1-4d45-b73c-6a1813382595",
   -- destinations={"Sutep"},
   credits=325,
   success_events={"find_dorath"},

   check_prereq=function(ship)
      if(require("lume").find(ship.upgrade_names, "life_support")) then
         return true
      else
         return false, "Cannot take mission without onboard life support sytem."
      end
   end,

   on_accept = function(ship)
      mail.deliver_msg(ship, "find-dorath-accept.msg")
      mission.record_event(ship, "find_dorath_accept")
   end,

   check_success = function(ship)
      return false -- TODO: make this mission winnable
   end,
}
