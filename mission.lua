-- missions are accepted by replying to newsgroup postings; see news.lua.

local utils = require("utils")

local fail = function(ship, mission, aborted)
   ship.credits = ship.credits - (mission.fail_credits or 0)
   if(mission.fail_function) then
      mission.fail_function(ship, aborted)
   end
   if(not aborted and mission.fail_message) then
      ship.api.print("Mission failed: " .. mission.fail_message)
   end
   for good, amt in pairs(mission.cargo or {}) do
      ship.cargo[good] = ship.cargo[good] - amt
   end
   ship.active_missions[mission.id] = nil
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

local destination_check = function(record)
   return (not record.destinations or #record.destinations == 0)
end

local record_destination = function(record, dest, ship)
   if(record.destinations and record.destinations[1] == dest) then
      table.remove(record.destinations, 1)
      if(record.msgs[dest]) then
         ship.api.print(record.msgs[dest])
      end
   end
end

local record_event = function(ship, e)
   ship.events[e] = utils.time(ship)
end

local find = function(id)
   return id and love.filesystem.isFile("data/missions/" .. id .. ".lua") and
      require("data.missions." .. id)
end

local on_login = function(ship)
   for mission_id,record in pairs(ship.active_missions) do
      local mission = find(mission_id)
      record_destination(record, ship.comm_connected, ship)
      if(mission.on_login) then mission.on_login(ship, ship.comm_connected) end

      -- success check here (maybe belongs in update)
      if((not mission.time_limit or
             utils.time(ship) < record.start_time + mission.time_limit) and
            cargo_check(ship, mission) and objectives_check(ship, mission) and
         destination_check(record)) then
         if(mission.success_function) then mission.success_function(ship) end
         for _,e in ipairs(mission.success_events or {}) do
            record_event(ship, e)
         end
         for good, amt in pairs(mission.cargo or {}) do
            ship.cargo[good] = ship.cargo[good] - amt
            assert(ship.cargo[good] >= 0, "Negative cargo amount.")
         end

         ship.credits = ship.credits + (mission.credits or 0)
         ship.api.print("Mission success: " .. (mission.success_message or "OK."))
         ship.active_missions[mission_id] = nil
      end
   end
end

local accept = function(ship, message_id)
   -- TODO: don't allow accept of missions that haven't actually been delivered
   local mission = find(message_id)
   if(not mission) then return false, "No mission " .. message_id end

   for _,event in pairs(mission.success_events or {}) do
      if(ship.events[event]) then
         return false, "Already completed this mission."
      end
   end

   if(mission.prereq) then
      local accept, msg = mission.prereq(ship)
      if(not accept) then return false, msg end
   end

   if(mission.cargo) then
      local sum = utils.sum(lume.values(mission.cargo))
      if(sum > (ship.cargo_capacity - ship:cargo_mass())) then
         return false, "Insufficient free cargo space"
      end
   end

   ship.active_missions[mission.id] = { start = utils.time(ship),
                                        destinations =
                                           lume.clone(mission.destinations or {}),
                                        msgs = mission.destination_msgs or {},
                                      }

   for good, amt in pairs(mission.cargo or {}) do
      ship:move_cargo(good, amt)
   end

   if(mission.accept_function) then
      mission.accept_function(ship)
   end
   return true, "Mission accepted."
end

local update = function(ship, dt)
   for mission_id,record in pairs(ship.active_missions) do
      local mission = find(mission_id)
      if(mission.update) then mission.update(ship, dt) end
      if(mission.time_limit and (utils.time(ship) >
                                 record.start_time + mission.time_limit)) then
         ship.api.print("Mission time limit exceeded: " .. mission.name)
         fail(ship, mission)
      end
   end
end

local list = function(ship)
   for mission_id in pairs(ship.active_missions) do
      local mission = require(mission_id)
      ship.api.print("\n")
      ship.api.print(mission.name)
      if(mission.description) then
         ship.api.print(mission.description)
      end
      if(mission.credits) then
         ship.api.print("Credits: " .. mission.credits)
      end
   end
end

local abort = function(ship, mission_name)
   local mission
   for mission_id in pairs(ship.active_missions) do
      local this_mission = require(mission_id)
      if(this_mission.name == mission_name) then
         mission = this_mission
      end
   end

   if(mission) then
      ship.active_missions[mission.id] = nil
      ship.api.print("Mission " .. mission.id .. " aborted.")
      fail(ship, mission, true)
   else
      ship.api.print("Mission " .. mission.id .. " not active.")
   end
end

local readout = function(ship)
   if(lume.count(ship.active_missions) == 0) then return "\n- none" end
   local s = ""
   for mission_id in pairs(ship.active_missions) do
      local this_mission = find(mission_id)
      s = s .. "\n- " .. this_mission.name
   end
   return s
end

return {
   accept = accept,
   on_login = on_login,
   update = update,
   list = list,
   abort = abort,
   readout = readout,
   find = find,
   record_event = record_event,
}
