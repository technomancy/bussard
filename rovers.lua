local utils = require("utils")
local lume = require("lume")
local os = require("os.rover")

local make = function(ship, target, rover_type, n)
   local is_saved_field = function(x)
      return type(x) ~= "function" and x ~= os
   end

   return {
      x = target.x, y = target.y, dx = 0, dy = 0, mass = 0,
      name = n and "Rover " .. n,
      update = function(r) utils.copy_keys(r, target, "x", "y", "dx", "dy") end,
      draw = function() end,
      rover_type = rover_type,
      landed_on = target.name,
      rover = true,
      os = os,
      to_save = function(r) return lume.filter(r, is_saved_field, true) end,
   }
end

local rovers = {
   deploy = function(ship, rover_type)
      if(not ship.target) then
         return ship.api.print("No target")
      elseif(not rover_type) then
         return ship.api.print("Usage: deploy(rover_type)")
      elseif(utils.distance(ship, ship.target) > ship.comm_range) then
         return ship.api.print("Out of range")
      elseif(not ship.rover_clearance[ship.target.name]) then
         return ship.api.print("Not cleared to land on", ship.target.name)
      elseif((ship.rovers[rover_type] or 0) < 1) then
         return ship.api.print("Out of stock of", rover_type)
      end
      ship.api.print(string.format("Landed a %s rover on %s.",
                                   rover_type, ship.target.name))
      local r = make(ship, ship.target, rover_type, ship.rovers[rover_type])
      table.insert(ship.bodies, r)
      ship.rovers[rover_type] = ship.rovers[rover_type] - 1
      ship.target, ship.target_number = r, lume.find(ship.bodies, r)
      return ship.api.editor.invisible
   end,

   recover = function(ship)
      if(utils.distance(ship, ship.target) > ship.comm_range) then
         return ship.api.print("Out of range")
      elseif(not ship.target.rover) then
         return ship.api.print("Target is not a rover")
      end
      local r = ship.target
      lume.remove(ship.bodies, r)
      ship.target, ship.target_number = nil, 0
      ship.rovers[r.rover_type] = ship.rovers[r.rover_type] + 1
      ship.api.print("Recovered", ship.target.name)
      return ship.api.editor.invisible
   end,

   list = function(ship)
      if(lume.count(ship.rovers) == 0) then
         ship.api.print("No rovers in stock")
      end
      for rover_type, count in pairs(ship.rovers) do
         ship.api.print(rover_type, ":", count)
      end
   end,

   make = make,
}

return rovers
