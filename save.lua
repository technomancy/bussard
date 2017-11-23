local lume = require("lume")
local body = require("body")
local ai = require("ship.ai")
local utils = require("utils")
local rovers = require("rovers")
local ship_init = require("data.ship_init")
local mission = require("mission")

local ship_fields = {
   "x", "y", "dx", "dy", "heading", "mail_delivered", "locked_to",
   "battery", "fuel", "credits", "system_name", "active_missions",
   "humans", "humans_left_at", "upgrade_names", "cargo", "target_number",
   "events", "flag", "name", "rovers", "rover_clearance", "loan", "fine",
}

local body_fields = {
   "x", "y", "dx", "dy", "cargo", "name", "mass", "image_name",
   "prices", "upgrade_prices", "fuel_price", "account_price",
   "progress", "from_name", "target_name", "ship", "asteroid", "strength",
}

local ship_filename = "ship_data.lua"
local system_filename = "system_data.lua"

local body_data = function(b)
   if(b.to_save) then
      return b:to_save()
   else
      return lume.pick(b, unpack(body_fields))
   end
end

return {
   save = function(ship)
      if(ship.aborting) then return end
      local ship_data = lume.pick(ship, unpack(ship_fields))
      ship_data.time_offset = ship.sandbox.os.time()
      ship_data.api = lume.pick(ship.api, unpack(ship.api.persist))
      ship_data.meta = {version = ship.api.ui.version}
      if(ship.api.cheat) then ship_data.api.cheat = true end

      love.filesystem.write(ship_filename, lume.serialize(ship_data))
      love.filesystem.write(system_filename,
                            lume.serialize(lume.map(ship.bodies, body_data)))

      for _,buffer in ipairs(ship.api.persist_buffers or {}) do
         local path = "buffers/" .. buffer
         love.filesystem.createDirectory("buffers")
         love.filesystem.write(path, ship.api.editor.dump_buffer(buffer))
      end
   end,

   load_into = function(ship)
      ship.load_time = os.time()

      if(love.filesystem.isFile(ship_filename)) then
         local ship_data_string = love.filesystem.read(ship_filename)
         local ship_data = lume.deserialize(ship_data_string)
         local api_data = ship_data.api

         lume.extend(ship.api, api_data)
         if(ship.api.cheat) then ship.api.cheat = ship end

         if((ship_data.meta or {}).version ~= ship.api.ui.version) then
            print("WARNING: you are loading an old save from version " ..
                     (ship_data.meta and ship_data.meta.version))
            print("There are likely to be incompatibilities; " ..
                     "press ctrl-f1 to reset config.")
         end

         ship_data.api, ship_data.meta = nil, nil
         lume.extend(ship, ship_data)

         -- when we are testing a single file but don't want to lose state
         if(os.getenv("BUSSARD_RESET_SRC")) then
            for _,s in pairs(lume.split(os.getenv("BUSSARD_RESET_SRC"), ",")) do
               ship.api.src[s] = love.filesystem.read("data/src/" .. s)
            end
         end
         ship:enter(ship.system_name, false, true)
      else
         ship_init(ship)
         ship:enter(ship.system_name, true, true)
      end
      if(love.filesystem.isFile(system_filename)) then
         local system_data_string = love.filesystem.read(system_filename)
         local system_data = lume.deserialize(system_data_string)
         for _,data in ipairs(system_data) do
            local existing = utils.find_by(ship.bodies, "name", data.name)
            if(existing) then
               lume.extend(existing, data)
            elseif(data.ship) then
               local other = ai.make(ship, ship.bodies, data.name)
               lume.extend(other, data)
               table.insert(ship.bodies, other)
            elseif(data.rover) then
               local target
               for _,b in pairs(ship.bodies) do
                  if(b.name == data.landed_on) then
                     target = b
                  end
               end
               local rover = rovers.make(ship, target)
               lume.extend(rover, data)
               table.insert(ship.bodies, rover)
            elseif(data.asteroid) then
               table.insert(ship.bodies, data)
            else -- if(data.world) then
               print("Discarding unknown system body", data.name)
            end
         end
         ship:recalculate()
      else
         ship:enter(ship.system_name, true, true)
      end
      for _,b in ipairs(love.filesystem.getDirectoryItems("buffers")) do
         local dumped = love.filesystem.read("buffers/" .. b)
         ship.api.editor.load_buffer(ship.api, dumped)
      end
      ship:dofile("src.config")
      ship.api.editor.open(ship.api, "*flight*", "flight")
      ship.api.editor.open(ship.api, "*console*")
      ship.api.editor.print_prompt()
      mission.init_active(ship)
      for system_name,s in pairs(ship.systems) do
         for _,b in pairs(s.bodies) do
            b.system, b.gov = system_name, s.gov
            if(b.portal) then assert(b.os, "OS-less portal") end
            if(b.fixed) then assert(not b.os, "OS on a fixed body") end

            if(system_name == ship.system_name and
               not (b.x and b.y and b.dx and b.dy)) then
               body.set_orbit(b, s.bodies[1])
            end
         end
      end
   end,

   abort = function(ship)
      love.filesystem.remove(ship_filename)
      love.filesystem.remove(system_filename)
      local function rm_rf(dir)
         for _,base in ipairs(love.filesystem.getDirectoryItems(dir)) do
            local entry = dir .. "/" .. base
            if(love.filesystem.isFile(entry)) then
               love.filesystem.remove(entry)
            elseif(love.filesystem.isDirectory(entry)) then
               rm_rf(entry)
               love.filesystem.remove(entry)
            end
         end
      end
      rm_rf("fs")
      rm_rf("buffers")
      rm_rf("rovers")

      ship.aborting = true
   end,

   backup = function(path)
      local save_dir = love.filesystem.getSaveDirectory()
      os.execute("cp -r " .. save_dir .. " " .. save_dir .. "/../" .. path)
   end,

   config_reset = function(ship)
      ship.api.src.bak = ship.api.src.bak or {}
      for _,v in pairs(love.filesystem.getDirectoryItems("data/src")) do
         ship.api.src.bak[v] = ship.api.src[v]
         ship.api.src[v] = love.filesystem.read("data/src/" .. v)
      end
      ship.api.src.config = "-- Your -*- lua -*- config has been reverted.\n" ..
         "-- The original files have been moved to the ship.src.bak directory."
         .. "\n\n" .. ship.api.src.config
      ship.api.print("Backed up config and restored to stock settings.")
      ship:configure(ship.systems, ship.api.ui)
      ship.api.dofile("src.config")
   end,

}
