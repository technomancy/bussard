local lume = require "lume"

local fields = {
   "x", "y", "dx", "dy", "heading",
   "fuel", "credits", "system_name",
   "upgrades", "cargo", "config", "target_number"
}

local filename = "savegame.lua"

return {
   save = function(ship, ui)
      local ship_fields = lume.pick(ship, unpack(fields))
      local game_fields = {scale = ui.scale}
      love.filesystem.write(filename, lume.serialize({ship_fields, game_fields}))
   end,

   load_into = function(ship, ui)
      if(love.filesystem.exists(filename)) then
         local data = love.filesystem.read(filename)
         local ship_fields, game_fields = unpack(lume.deserialize(data))
         lume.extend(ship, ship_fields)
         ui.scale = game_fields.scale
         ship.target = ship.api.sensors.bodies[ship.target_number]
      end
   end,
}
