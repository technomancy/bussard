local lume = require "lume"

local ship_fields = {
   "x", "y", "dx", "dy", "heading",
   "fuel", "credits", "system_name",
   "upgrades", "cargo", "config", "target_number"
}

local system_fields = {
   "x", "y", "dx", "dy", "cargo"
}

local ship_filename = "ship_data.lua"
local system_filename = "system_data.lua"

local system_data = function(bodies)
   local data = {}
   for i,b in ipairs(bodies) do
      data[b.name] = lume.pick(b, unpack(system_fields))
   end
   return data
end

return {
   save = function(ship, ui)
      -- TODO: write ship config
      -- TODO: write filesystems
      local ship_data = lume.pick(ship, unpack(ship_fields))
      love.filesystem.write(ship_filename, lume.serialize(ship_data))
      love.filesystem.write(system_filename,
                            lume.serialize(system_data(ship.bodies)))
   end,

   load_into = function(ship)
      if(love.filesystem.exists(ship_filename)) then
         local ship_data_string = love.filesystem.read(ship_filename)
         lume.extend(ship, lume.deserialize(ship_data_string))
         ship.target = ship.bodies[ship.target_number]
      end
      if(love.filesystem.exists(system_filename)) then
         local system_data_string = love.filesystem.read(system_filename)
         local system_data = lume.deserialize(system_data_string)
         for i,b in ipairs(ship.bodies) do
            lume.extend(b, system_data[b.name])
         end
      end
   end,

   abort = function()
      love.filesystem.remove(ship_filename)
   end,
}
