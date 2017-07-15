-- missions are accepted by replying to messages; see mail.lua

local recover = require("data.recover")
local utils = require("utils")
local lume = require("lume")

-- # Mission structure

-- callbacks:
-- * init(ship): runs when accepted and when loading saved games
-- * on_accept(ship)
-- * check_prereq(ship) -> boolean
-- * update(ship, dt)
-- * on_login(ship, connected_name)
-- * check_success(ship) -> boolean
-- * on_success(ship)
-- * on_fail(ship)

-- other optional fields:
-- * description: required for non-invisible
-- * name: required for non-invisible
-- * time_limit: number of in-game seconds allowed to finish
-- * credits: to gain upon success
-- * destinations: a table of body names you must visit, in order
-- * destination_msgs: table of body names->messages to show when logging in
-- * cargo: table of goods->tons that need to be delivered
-- * objectives: array of events that need to have happened to succeed
-- * success_events: array of events to set upon success
-- * success_message: prints upon mission success
-- * fail_message: prints upon mission failure
-- * invisible: don't show in mission listing

local fail = function(ship, mission, aborted)
   if(mission.on_fail) then
      mission.on_fail(ship, aborted)
   end
   if(not aborted and not mission.invisible) then
      ship.api.print("Mission failed: " .. mission.fail_message or mission.name)
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
   ship.events[e] = ship.events[e] or utils.time(ship)
end

local find = function(ship, id)
   -- recovery missions are special-cased: when you sell your life-support
   -- system and have a companion human on board, a recovery mission is
   -- generated to remind you that you need to come back to that planet and pick
   -- them back up once you purchase life support again.
   if(id and id:match("^recover")) then return recover(ship, id) end
   if(id and love.filesystem.isFile("data/missions/" .. id .. ".lua")) then
      local m = require("data.missions." .. id)
      m.id = id
      return m
   end
end

local check_success = function(ship, mission)
   local record = ship.active_missions[mission.id]
   if(mission.time_limit and (utils.time(ship) >
                              record.start_time + mission.time_limit)) then
      ship.api.print("Mission time limit exceeded: " .. mission.name)
      fail(ship, mission)
      return false
   end

   return cargo_check(ship, mission) and objectives_check(ship, mission) and
      (not mission.check_success or mission.check_success(ship)) and
      destination_check(record)
end

local succeed = function(ship, mission)
   if(mission.on_success) then mission.on_success(ship) end
   lume.map(mission.success_events or {}, lume.fn(record_event, ship))
   for good, amt in pairs(mission.cargo or {}) do
      ship.cargo[good] = ship.cargo[good] - amt
      assert(ship.cargo[good] >= 0, "Negative cargo amount.")
   end

   ship.credits = ship.credits + (mission.credits or 0)
   if(not mission.invisible) then
      ship.api.print("Mission success: " .. (mission.success_message or "OK."))
   end
   ship.active_missions[mission.id] = nil
end

local on_login = function(ship)
   for mission_id,record in pairs(ship.active_missions) do
      local mission = find(ship, mission_id)
      record_destination(record, ship.target.name, ship)
      if(mission.on_login) then
         mission.on_login(ship, ship.target.name, record)
      end
      if(check_success(ship, mission)) then succeed(ship, mission) end
   end
end

local accept = function(ship, message_id)
   local mission = find(ship, message_id)
   if(not mission) then return false, "No mission " .. message_id end

   if(ship.active_missions[mission.id]) then
      return false, "Mission is in progress."
   end

   for _,event in pairs(mission.success_events or {}) do
      if(ship.events[event]) then
         return false, "Already completed this mission."
      end
   end

   if(mission.check_prereq) then
      local accept, msg = mission.check_prereq(ship)
      if(not accept) then return false, msg end
   end

   if(mission.cargo) then
      local sum = utils.sum(lume.values(mission.cargo))
      if(sum > (ship.cargo_capacity - ship:cargo_mass())) then
         return false, "Insufficient free cargo space"
      end
   end

   -- mission record
   local record = { start = utils.time(ship),
                    destinations =
                       lume.clone(mission.destinations or {}),
                    msgs = mission.destination_msgs or {},
                  }
   ship.active_missions[mission.id] = record

   for good, amt in pairs(mission.cargo or {}) do
      ship:move_cargo(good, amt)
   end

   if(mission.on_accept) then mission.on_accept(ship) end
   if(mission.init) then mission.init(ship, record) end
   return true, "Mission accepted."
end

local update = function(ship, dt)
   for mission_id in pairs(ship.active_missions) do
      local mission = find(ship, mission_id)
      if(mission.update) then mission.update(ship, dt) end
      if(check_success(ship, mission)) then succeed(ship, mission) end
   end
end

local list = function(ship)
   if(lume.count(ship.active_missions) == 0) then
      ship.api.print("No missions.")
   else
      for mission_id in pairs(ship.active_missions) do
         local mission = find(ship, mission_id)
         if(not mission.invisible) then
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
   end
end

local abort = function(ship, mission_name)
   local mission
   for mission_id in pairs(ship.active_missions) do
      local this_mission = find(ship, mission_id)
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
      local this_mission = find(ship, mission_id)
      if(not this_mission.invisible) then
         s = s .. "\n- " .. this_mission.name
      end
   end
   return s
end

local init_active = function(ship)
   for mission_id,record in pairs(ship.active_missions) do
      local this_mission = find(ship, mission_id)
      if(this_mission.init) then
         this_mission.init(ship, record)
      end
   end
end

local wait_for = function(ship, event)
   assert(ship.ship)
   if(type(event) == "string") then
      while(not ship.events[event]) do coroutine.yield() end
   else
      while(not event(ship)) do coroutine.yield() end
   end
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
   init_active = init_active,
   wait_for = wait_for,
}
