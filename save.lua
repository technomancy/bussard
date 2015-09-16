local lume = require "lume"

local ship_fields = {
   "x", "y", "dx", "dy", "heading",
   "fuel", "credits", "system_name",
   "upgrades", "cargo", "config", "target_number",
   -- TODO: way to save off subtable things like scale
}

local system_fields = {
   "x", "y", "dx", "dy", "cargo"
}

local ship_filename = "ship_data.lua"
local config_filename = "ship_config.lua"
local system_filename = "system_data.lua"

local get_system_data = function(bodies)
   local data = {}
   for _,b in ipairs(bodies) do
      data[b.name] = lume.pick(b, unpack(system_fields))
   end
   return data
end

return {
   save = function(ship)
      -- TODO1: persist arbitrary ship.api fields
      -- TODO: write filesystems
      local ship_data = lume.pick(ship, unpack(ship_fields))
      ship_data.scale = ship.api.scale
      ship_data.config = ship.api.config
      love.filesystem.write(ship_filename, lume.serialize(ship_data))
      love.filesystem.write(system_filename,
                            lume.serialize(get_system_data(ship.bodies)))
   end,

   load_into = function(ship)
      if(love.filesystem.exists(ship_filename)) then
         local ship_data_string = love.filesystem.read(ship_filename)
         local ship_data = lume.deserialize(ship_data_string)
         ship.api.scale = ship_data.scale or ship.api.scale
         ship.api.config = ship_data.config or ship.api.config
         lume.extend(ship, ship_data)
         ship.target = ship.bodies[ship.target_number]
         ship:enter(ship.system_name)
      end
      if(love.filesystem.exists(system_filename)) then
         local system_data_string = love.filesystem.read(system_filename)
         local system_data = lume.deserialize(system_data_string)
         for _,b in ipairs(ship.bodies) do
            lume.extend(b, system_data[b.name])
         end
      end
   end,

   abort = function()
      love.filesystem.remove(ship_filename)
   end,
}
