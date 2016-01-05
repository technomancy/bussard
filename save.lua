local lume = require "lume"
local body = require "body"
local other_ship = require "ship.others"
local utils = require "utils"

local ship_fields = {
   "x", "y", "dx", "dy", "heading",
   "battery", "fuel", "credits", "system_name", "active_missions",
   "upgrade_names", "cargo", "target_number", "events", "flag",
}

local body_fields = {
   "x", "y", "dx", "dy", "cargo", "name", "mass",
   "prices", "upgrade_prices", "fuel_price", "account_price",
   "progress", "from_name", "target_name", "ship", "asteroid",
}

local ship_filename = "ship_data.lua"
local system_filename = "system_data.lua"

local fs_filename = function(b)
   return "fs/" .. b.name .. ".lua"
end

local body_data = function(b)
   return lume.pick(b, unpack(body_fields))
end

return {
   save = function(ship)
      local ship_data = lume.pick(ship, unpack(ship_fields))
      ship_data.time_offset = ship.api.repl.sandbox.os.time()
      ship_data.api = lume.pick(ship.api, unpack(ship.api.persist))
      love.filesystem.write(ship_filename, lume.serialize(ship_data))
      love.filesystem.write(system_filename,
                            lume.serialize(lume.map(ship.bodies, body_data)))
      love.filesystem.createDirectory("fs")
      for _,s in pairs(ship.systems) do
         for _,b in pairs(s.bodies) do
            local fs = body.filesystems[b.name]
            if(fs) then
               orb.fs.strip_special(fs)
               local fs_data = lume.serialize(fs)
               love.filesystem.write(fs_filename(b), fs_data)
            end
         end
      end
   end,

   load_into = function(ship)
      ship.load_time = os.time()
      if(love.filesystem.isFile(ship_filename)) then
         local ship_data_string = love.filesystem.read(ship_filename)
         local ship_data = lume.deserialize(ship_data_string)
         local api_data = ship_data.api

         lume.extend(ship.api, api_data)
         ship_data.api = nil

         lume.extend(ship, ship_data)
         ship:enter(ship.system_name)
         ship.api.repl.display_line = nil
      else
         ship.time_offset = 8383504000
         ship:enter(ship.system_name, true)
      end
      if(love.filesystem.isFile(system_filename)) then
         local system_data_string = love.filesystem.read(system_filename)
         local system_data = lume.deserialize(system_data_string)
         for i,data in ipairs(system_data) do
            local existing = utils.find_by(ship.bodies, "name", data.name)
            if(existing) then
               lume.extend(existing, data)
            elseif(data.ship) then
               local other = other_ship.make(ship.bodies, data.name)
               lume.extend(other, data)
               ship.bodies[i] = other
            else
               ship.bodies[i] = data
            end
         end
      else
         ship:enter(ship.system_name, true)
      end
      for _,s in pairs(ship.systems) do
         for _,b in pairs(s.bodies) do
            if(love.filesystem.isFile(fs_filename(b))) then
               local fs_data = love.filesystem.read(fs_filename(b))
               body.filesystems[b.name] = lume.deserialize(fs_data)
            end
         end
      end
   end,

   abort = function(ship)
      love.filesystem.remove(ship_filename)
      love.filesystem.remove(system_filename)
      for _,s in pairs(ship.systems) do
         for _,b in pairs(s.bodies) do
            love.filesystem.remove(fs_filename(b))
         end
      end
   end,}
