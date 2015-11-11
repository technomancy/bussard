local utils = require("utils")

local fail = function(ship, mission)
   ship.credits = ship.credits - (mission.fail_credits or 0)
   if(mission.fail_function) then
      mission.fail_function(ship)
   end
end

local cargo_check = function(ship, mission)
   for good, amt in pairs(mission.cargo or {}) do
      if((ship.cargo[good] or 0) < amt) then
         return false
      end
   end
   return true
end

local objectives_check = function(ship, mission)
   for _,objective in ipairs(mission.objectives or {}) do
      if(not ship.events[objective]) then return false end
   end
   return true
end

local check = function(ship)
   print("checking missions")
   for mission_id,start_time in pairs(ship.active_missions) do
      local mission = require("data.missions." .. mission_id)
      print(cargo_check(ship, mission))
      print(objectives_check(ship, mission))
      if((not mission.time_limit or
             utils.time(ship) < start_time + mission.time_limit) and
         cargo_check(ship, mission) and objectives_check(ship, mission)) then
         if(mission.success_function) then mission.success_function(ship) end
         for _,e in ipairs(mission.success_events or {}) do
            ship.events[e] = true
         end
         ship.credits = ship.credits + (mission.credits or 0)
         ship.api.repl.print("Mission success: " .. mission.success_message)
      end
   end
end

local accept = function(ship, message_id)
   local mission = require("data.missions." .. message_id)
   if(not mission) then
      return false, "no mission."
   else
      ship.active_missions[message_id] = utils.time(ship)
      for good, amt in pairs(mission.cargo or {}) do
         ship.cargo[good] = (ship.cargo[good] or 0) + amt
      end
      return true
   end
end

local update = function(ship)
   for mission_id,start_time in ipairs(ship.active_missions) do
      local mission = require("data.missions." .. mission_id)
      if(mission.updater) then mission.updater(ship) end
      if(utils.time(ship) > start_time + mission.time_limit) then
         ship.api.repl.print("Mission time limit exceeded: " .. mission.name)
         fail(ship, mission)
      end
   end
end

local list = function(ship)
   for mission_id,started_at in pairs(ship.active_missions) do
      local mission = require("data.missions." .. mission_id)
      ship.api.repl.print("\n")
      ship.api.repl.print(mission.name)
      if(mission.description) then
         ship.api.repl.print(mission.description)
      end
      if(mission.credits) then
         ship.api.repl.print("Credits: " .. mission.credits)
      end
   end
end

local abort = function(ship, mission_name)
   local mission
   for mission_id,timestamp in pairs(ship.active_missions) do
      local this_mission = require("data.missions." .. mission_id)
      if(this_mission.name == mission_name) then
         mission = this_mission
      end
   end

   if(mission) then
      local mission = require("data.missions." .. mission_id)
      ship.active_missions[mission_id] = nil
      ship.api.repl.print("Mission " .. mission_id .. " aborted.")
      fail(ship, mission)
   else
      ship.api.repl.print("Mission " .. mission_id .. " not active.")
   end
end

return {
   accept = accept,
   check = check,
   update = update,
   list = list,
   abort = abort,
}
