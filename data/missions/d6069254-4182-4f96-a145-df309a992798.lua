-- -*- lua -*-
local mail = require("mail")

return {
   name="passenger2",
   description="Passenger run from Tana Prime to Apkabar Station",
   id="d6069254-4182-4f96-a145-df309a992798",
   destinations={"Tana Prime", "Newton Station"},
   credits=250,
   success_events={"passenger2"},

   check_prereq=function(ship)
      if(require("lume").find(ship.upgrade_names, "life_support")) then
         return true
      else
         return false, "Cannot take passengers without onboard life support sytem."
      end
   end,

   on_login = function(ship, target)
      local m = ship.active_missions["d6069254-4182-4f96-a145-df309a992798"]
      if(not m.pickup and target == "Tana Prime") then
         m.pickup = 0
      end
   end,

   on_accept = function(ship)
      mail.deliver_msg(ship, "nari-a-01.msg")
      local m = ship.active_missions["d6069254-4182-4f96-a145-df309a992798"]
      if(ship.comm_connected == "Tana Prime") then m.pickup = 0 end
   end,

   update = function(ship, dt)
      local m = ship.active_missions["d6069254-4182-4f96-a145-df309a992798"]
      if(not m or not m.pickup) then return end
      m.pickup = m.pickup + dt
      if(m.pickup > 12 and not ship.mail_delivered["nari-a-02"]) then
         mail.deliver_msg(ship, "nari-a-02.msg")
      elseif(m.pickup > 32 and not ship.mail_delivered["nari-a-03"]) then
         mail.deliver_msg(ship, "nari-a-03.msg")
      elseif(ship.events.invite_nari and not ship.humans.nari and
             ship.comm_connected == "Tana Prime") then
         ship.humans.nari = "companion"
         mail.deliver_msg(ship, "nari-a-05.msg")
      end
   end,

   on_success = function(ship)
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
-- 09: on completion
