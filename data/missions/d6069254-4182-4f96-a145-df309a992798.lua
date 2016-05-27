local mail = require("mail")
local mission = require("mission")

return {
   name="passenger2",
   description="Passenger run from Tana Prime to Apkabar Station",
   id="d6069254-4182-4f96-a145-df309a992798",
   destinations={"Tana Prime", "Newton Station"},
   destination_msgs={["Tana Prime"] = "Nari Phouen has boarded."},
   credits=250,
   success_events={"passenger2"},

   prereq=function(ship)
      if(require("lume").find(ship.upgrade_names, "life_support")) then
         return true
      else
         return false, "Cannot take passengers without onboard life support sytem."
      end
   end,

   -- TODO: start countdown when accepting the mission if you're already logged in
   on_login = function(ship, target)
      local m = ship.active_missions["d6069254-4182-4f96-a145-df309a992798"]
      if(not m.pickup and target == "Tana Prime") then
         m.pickup = 0
      elseif(target == "Interportal: Sol" and not ship.events.try_interportal) then
         mail.deliver_msg(ship, "nari-a-07.msg")
         mission.record_event(ship, "try_interportal")
      elseif(target == "Apkabar Station" and ship.events.try_interportal
             and not ship.events.background_check) then
         mail.deliver_msg(ship, "nari-a-08.msg")
         mission.record_event(ship, "background_check")
      elseif(ship.events.invite_nari and not ship.mail_delivered["nari-a-05"] and
             target == "Tana Prime") then
         mail.deliver_msg(ship, "nari-a-05.msg")
      end
   end,

   accept_function = function(ship)
      mail.deliver_msg(ship, "nari-a-01.msg")
   end,

   update = function(ship, dt)
      local m = ship.active_missions["d6069254-4182-4f96-a145-df309a992798"]
      if(not m or not m.pickup) then return end
      m.pickup = m.pickup + dt
      if(m.pickup > 12 and not ship.mail_delivered["nari-a-02"]) then
         mail.deliver_msg(ship, "nari-a-02.msg")
      elseif(m.pickup > 32 and not ship.mail_delivered["nari-a-03"]) then
         mail.deliver_msg(ship, "nari-a-03.msg")
      end
   end,

   success_function = function(ship)
      mission.record_event(ship, "luabook")
      mail.deliver_msg(ship, "nari-a-09.msg")
   end,
}

-- messages:
-- 01: on accept
-- 02: timed after accept
-- 03: timed after accept
-- 04: on ack 03
-- 05: on ack 04
-- 06: on ack 05
-- 07: on attempt to interportal
-- 08: on login to apkabar
-- 09: on completion
