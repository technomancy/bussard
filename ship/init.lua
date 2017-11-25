local utils = require("utils")
local lume = require("lume")
local serpent = require("serpent")

local help = require("doc")
local upgrade = require("data.upgrades")
local ai = require("ship.ai")
local client = require("os.client")
local rovers = require("rovers")
local mail = require("mail")
local mission = require("mission")
local host_fs_proxy = require("host_fs_proxy")

-- for shuffling systems upon entry
local asteroid = require("asteroid")
local body = require("body")
local systems = require("data.systems")

local editor = require("polywell")

local with_traceback = lume.fn(utils.with_traceback, editor.print)

local scale_min = 2

local status_whitelist = {
   "x", "y", "dx", "dy", "heading", "target", "system_name", "bodies",
   "fuel", "fuel_capacity", "battery", "battery_capacity", "mass",
   "engine_on", "turning_right", "turning_left", "credits", "upgrade_names",
   "cargo", "cargo_capacity", "solar",
   "engine_strength", "turning_speed", "cpuinfo",
   "recharge_rate", "burn_rate", "comm_connected", "comm_range",
   "portal_range", "portal_time", "flag", "target", "comm_boost", "time_factor",
}

local base_stats = {
   mass = 128,
   cargo_capacity = 64,
   fuel_capacity = 192,
   comm_range = 2048,
   recharge_rate = 1/4,
   burn_rate = 1,
   engine_strength = 512,
   turning_speed = 1/2,
   battery_capacity = 64,
   solar = 30,

   portal_range = 2048,
   portal_time = 40, -- in-game seconds
}

local sandbox_loadstring = function(ship, code, chunkname)
   local chunk, err = loadstring(code, chunkname)
   if(chunk) then
      setfenv(chunk, ship.sandbox)
      return chunk
   else
      return chunk, err
   end
end

local sandbox_dofile = function(ship, filename)
   local contents = ship.api:find(filename)
   assert(type(contents) == "string", filename .. " is not a file.")
   local chunk, err = sandbox_loadstring(ship, contents, filename)
   if(not err) then
      return chunk()
   else
      error(err)
   end
end

local sandbox_loaded = {}

local sandbox_require = function(ship, mod_name)
   if(sandbox_loaded[mod_name]) then return end
   sandbox_dofile(ship, mod_name)
   sandbox_loaded[mod_name] = true
end

local expose_system = function(sys)
   local fields = {"x", "y", "r", "mass", "name", "pop",
                   "portal", "world", "station", "star"}
   local bodies = lume.map(sys.bodies, utils.rfn(lume.pick, unpack(fields)))
   return lume.merge({bodies=bodies}, lume.pick(sys, "x", "y", "gov"))
end

local universe_api = utils.readonly_proxy {
   g = body.g,
   max_accel = body.max_accel,
   systems = lume.map(systems, expose_system),
}

local sandbox = function(ship)
   local serpent_opts = {maxlevel=8,maxnum=64,nocode=true,comment=false,}
   return lume.merge(utils.sandbox,
                     { help = help.message,
                       print = ship.api.editor.print,
                       ship = ship.api,
                       editor = editor,
                       universe = universe_api,
                       _LOADED = sandbox_loaded,
                       dofile = lume.fn(sandbox_dofile, ship),
                       require = lume.fn(sandbox_require, ship),
                       loadstring = lume.fn(sandbox_loadstring, ship),
                       debug = {traceback = debug.traceback},
                       os = {time = lume.fn(utils.time, ship)},
                       man = lume.fn(help.man, ship),
                       define_mode = editor.define_mode,
                       bind = editor.bind,
                       ssh_connect = lume.fn(client.connect, ship),
                       reply = lume.fn(mail.reply, ship),
                       replyable = mail.replyable,
                       graphics = love.graphics,
                       image = love.image,
                       hsv = utils.hsv,
                       flight_draw = require("draw"),
                       is_key_down = love.keyboard.isDown,
                       pps = function(x)
                          return serpent.block(x, serpent_opts) end,
                       pp = function(x)
                          editor.print(serpent.block(x, serpent_opts)) end,
                       ls = function(path)
                          for k,v in pairs(ship.api:find(path or ".")) do
                             ship.api.editor.print(k .. "   " .. type(v))
                          end
                          return ship.api.editor.invisible
                       end,
                       setmetatable = function(obj, t)
                         t.__gc = nil
                         t.__metatable_is_set_by_bussard_user = true
                         setmetatable(obj, t)
                       end,
                       getmetatable = function(obj)
                         local t = getmetatable(obj)
                         if ((type(t)=="table")
                             and t.__metatable_is_set_by_bussard_user) then
                           return t
                         else
                           return "Inaccessible metatable"
                         end
                       end,
                     })
end

local ship = {
   base_stats = base_stats,
   ship=true,

   -- ephemeral
   x=0, y=0, dx=0, dy=-200, heading = math.pi,
   engine_on = false,
   turning_right = false,
   turning_left = false,
   target_number = 0,
   target = nil,
   mass = 128,
   battery = 64,
   upgrades = {}, -- map of upgrade name -> upgrade map
   time_factor = 10,
   min_time_factor = 1,

   -- keep around
   fuel = 128,
   credits = 512,
   time_offset = utils.game_start,
   cargo = {}, -- map of cargo name -> tons number
   upgrade_names={}, -- array of upgrade names
   active_missions={}, -- map of mission id -> mission record map
   mail_delivered={}, -- map of message name -> true
   events={}, -- map of event names -> timestamps
   humans={}, -- array of human names
   humans_left_at={}, -- map of human name -> world name
   rovers={basic=0}, -- map of type -> number
   rover_clearance={}, -- map of world names -> true
   updaters={}, -- system-level coros
   loan=0,
   fine=0,

   cpuinfo = {processors=64, arch="arm128-ng", mhz=2800},
   configure = function(ship, sys, ui)
      ship.api.ui = ui
      ship.systems = sys

      ship.sandbox = sandbox(ship)
      ship.sandbox["_G"] = ship.sandbox
      ship.timer = utils.ptimer(40, function(dt)
                                   mission.update(ship, dt)
                                   ship:long_update(dt)
                                   mail.deliver(ship)
      end)
   end,

   dofile = sandbox_dofile,

   enter = function(ship, system_name, reseed, suppress_message)
      -- if something was waiting for a key release, let it get its
      -- event, as we are going to change context
      if love.keyreleased then love.keyreleased() end

      local from = ship.system_name
      if(from ~= ship.system_name) then
         for _,b in ipairs(ship.bodies) do body.stop(b) end
      end
      assert(ship.systems[system_name], system_name .. " not found.")
      if(not suppress_message) then
         ship.api.editor.print("Entering the " .. system_name .. " system.")
      end

      -- stuff these things in there to expose to in-ship APIs
      ship.bodies = ship.systems[system_name].bodies
      ship.system_name = system_name

      for _,b in ipairs(ship.bodies) do body.start(b) end

      ship:recalculate()

      if(reseed) then
         ship.target_number, ship.target = 0, nil

         -- re-seed system-level things
         asteroid.populate(ship.systems[ship.system_name])
         for _,b in pairs(ship.bodies) do
            body.set_orbit(b, ship.bodies[1])
            body.seed_cargo(b)
         end

         local portal = lume.match(ship.bodies,
                                   function(b) return b.portal == from end)
         if(portal) then
            body.set_orbit(ship, portal, 512)
            ship.target = portal
            ship.target_number = lume.find(ship.bodies, portal)
         else -- seed to bottom-left quadrant
            ship.r = 16831
            body.set_orbit(ship, ship.bodies[1])
            ship.x, ship.y = -(math.abs(ship.x)), math.abs(ship.y)
            ship.dx, ship.dy = math.abs(ship.dx), math.abs(ship.dy)
         end
         ai.seed(ship)
      end
      if(ship.api.on_enter) then ship.api.on_enter(system_name) end
   end,

   update = function(ship, dt, real_dt)
      ship.api.dt = dt

      -- activate controls
      if(editor.current_mode_name() == "flight") then
         for k,f in pairs(ship.api.controls) do
            with_traceback(f, love.keyboard.isDown(k), real_dt)
         end
      end

      -- out of sandbox
      utils.run_handlers(ship, "updaters", "broken_updaters", {ship, dt}, print)

      -- this seems overcomplicated at first glance--why are we
      -- setting an engine_on bit in the ship.controls handler and
      -- then checking it later? the answer is that otherwise it would
      -- be a sandbox violation. if the ship.controls handler affected
      -- acceleration directly, you could write code that would make
      -- the engine arbitrarily powerful or use zero fuel or
      -- whatever. so these two steps must remain separate.
      if(ship.engine_on and ship.fuel > 0 and not ship.locked_to) then
         local fx = (math.sin(ship.heading) * dt * ship.engine_strength)
         local fy = (math.cos(ship.heading) * dt * ship.engine_strength)
         ship.dx = ship.dx + fx / ship.mass
         ship.dy = ship.dy + fy / ship.mass
         ship.fuel = ship.fuel - (ship.burn_rate * dt)
      elseif(ship.fuel < ship.fuel_capacity) then
         ship.fuel = ship.fuel + (ship.recharge_rate * dt)
      end

      body.orbital_lock(ship, ship.locked_to)
      if(ship.locked_to and ship.engine_on) then
         local key = ship.api.editor.key_for(ship.api.orbital_lock)
         if(key) then
            ship.api.editor.print("Orbital lock engaged; disengage with "..
                                     key .. " to use engines.")
         end
      end

      if(ship.turning_left) then
         ship.heading = ship.heading + (dt * ship.turning_speed)
      elseif(ship.turning_right) then
         ship.heading = ship.heading - (dt * ship.turning_speed)
      end

      if(ship.battery < ship.battery_capacity) then
         local dist = utils.distance(ship.x, ship.y)
         ship.battery = ship.battery + (dt / math.log(dist*2)) * ship.solar
      end

      utils.run_handlers(ship.api, "updaters", "broken_updaters",
                         {ship.api, dt}, editor.print)

      for _,u in pairs(ship.upgrades) do
         if(u.update) then u.update(ship, dt) end
      end

      if(ship.api.scale < scale_min) then ship.api.scale = scale_min end
      ship.timer(dt)
   end,

   in_range = function(ship, b, range)
      return b and utils.distance(ship.x - b.x, ship.y - b.y) <
         (range or ship.comm_range)
   end,

   cargo_amount = function(ship)
      local amt = 0
      for _,v in pairs(ship.cargo) do amt = amt + v end
      return amt
   end,

   move_cargo = function(ship, good, amount, discard_remainder)
      assert((ship.cargo[good] or 0) >= -amount, "Not enough " .. good)
      local available_mass = ship.cargo_capacity - ship:cargo_mass()

      if(discard_remainder) then
         amount = math.min(amount, available_mass)
      else
         assert(amount <= available_mass, "Can't fit " .. amount .. " in hold.")
      end

      ship.cargo[good] = (ship.cargo[good] or 0) + amount
      ship:recalculate()
   end,

   -- run when cargo or upgrades change; always idempotent
   recalculate = function(ship)
      ship.target = ship.bodies[ship.target_number]

      for k,v in pairs(base_stats) do
         ship[k] = v
      end

      for _,u in ipairs(ship.upgrade_names) do
         ship.upgrades[u] = assert(upgrade[u], u .. " not found.")
         if(upgrade[u].load) then upgrade[u].load(ship) end
         for k,v in pairs(upgrade[u].stats or {}) do
            ship[k] = (ship[k] or 0) + v
         end
         ship.api.actions[u] = upgrade[u].action and
            lume.fn(upgrade[u].action, ship)
      end

      for good,amt in pairs(ship.cargo) do
         if(amt == 0) then ship.cargo[good] = nil end
      end

      ship.mass = ship.mass + ship:cargo_mass()
   end,

   cargo_mass = function(ship)
      local m = 0
      for _,c in pairs(ship.cargo) do m = m + c end
      return m
   end,

   long_update = function(ship, dt)
      asteroid.recycle(ship)
      utils.run_handlers(ship.api, "long_updaters", "broken_updaters",
         {ship.api, dt}, editor.print)
   end,

   remove_body = function(ship, b)
      lume.remove(ship.bodies, b)
      if(ship.target == b) then
         ship.target, ship.target_number = nil, 0
      end
   end,

   disembark = function(ship, human)
      local m = ship.humans[human]
      ship.humans[human] = nil
      if(m == "companion") then
         ship.humans_left_at[human] = assert(ship.target,
                                             "Can't disembark nowhere.")
         mission.accept(ship, "recover_" .. human)
         mail.deliver_msg(ship, "recover_" .. human .. ".msg")
      elseif(m) then
         mission.fail(ship, m, false)
      end
   end,

   -- for debugging during development: ship.cheat:realdofile("main.lua")
   realdofile = function(ship, x) dofile(x) ship.api.ui.play() end,
   realrequire = require,
   love = love,
   setmetatable = setmetatable,
   getmetatable = getmetatable,
}

-- everything in here is exposed to the sandbox. this table *is* `ship`, as far
-- as the in-game code is concerned.
ship.api = {
   editor = editor,
   help = help,

   mission = {
      list = lume.fn(mission.list, ship),
      abort = lume.fn(mission.abort, ship),
      hud = {x=-20, y=-100, type="text", format="Missions: %s",
             align="right", values={lume.fn(mission.readout, ship)}}
   },

   status = utils.whitelist_table(ship, status_whitelist, "status"),

   -- upgrades can place functions in this table when loaded
   actions = {
      forward = function(down) ship.engine_on = down end,
      left = function(down) ship.turning_left = down end,
      right = function(down) ship.turning_right = down end,
      next_target = function()
         if(love.keyboard.isDown("lshift", "rshift")) then
            ship.target_number = ((ship.target_number - 1) %
                  (#ship.bodies + 1))
         else
            ship.target_number = ((ship.target_number + 1) %
                  (#ship.bodies + 1))
         end
         ship.api.closest_cycle = 1
         ship.target = ship.bodies[ship.target_number]
      end,
      closest_target = function()
         ship.api.closest_cycle = ship.api.closest_cycle or 1
         local ts = utils.sort_by(ship.bodies, lume.fn(utils.distance, ship))
         local target = ts[ship.api.closest_cycle]
         for i,b in ipairs(ship.bodies) do
            if(b == target) then
               ship.target, ship.target_number = b, i
            end
         end
         ship.api.closest_cycle = ship.api.closest_cycle + 1
      end,
      select_target = function(t)
         if(type(t) == "string") then
            ship.target_number, ship.target =
               utils.find_kv_by(ship.bodies, "name", t)
         elseif(type(t) == "number") then
            ship.target, ship.target_number = ship.bodies[t], t
         else
            error("unknown target type: " .. t)
         end
      end,
   },

   find = function(s, path, use_rawget)
      if(path == ".") then return s end
      if(type(path) ~= "string") then return end
      if(path:find("^/")) then
         if(love.filesystem.isFile(path)) then
            return love.filesystem.read(path)
         else
            local parts = lume.split(path, "/")
            table.remove(parts, #parts)
            if(love.filesystem.isDirectory(table.concat(parts), "/")) then
               return ""
            else
               return s.editor.echo("Not a valid location")
            end
         end
      else
         local parts = lume.split(path, ".")
         local target = s
         for _,p in ipairs(parts) do
            if(use_rawget) then
               -- special-case to bypass metatable __index recursion
               target, use_rawget = rawget(target, p), false
            elseif(type(target) == "table" and p ~= "") then
               target = target[p]
            elseif(p ~= "") then
               return nil
            end
            -- print(p, target, use_rawget)
         end
         return target
      end
   end,

   set = function(s, path, contents)
      if(path:find("^/")) then
         assert(love.filesystem.write(path, contents))
      else
         local parts = lume.split(path, ".")
         local basename = table.remove(parts, #parts)
         local target = s
         for _,p in ipairs(parts) do
            if(type(target) == "table" and p ~= "") then
               target = target[p]
            elseif(p ~= "") then
               return nil
            end
         end
         rawset(target, basename, contents)
      end
   end,

   dofile = lume.fn(sandbox_dofile, ship),

   -- for user files
   src = {},
   docs = {},
   persist = {"persist", "scale", "src", "docs"},
   persist_buffers = {"*console*"},

   reload_file = function(name)
      ship.api.src[name] = love.filesystem.read("data/src/" .. name)
   end,

   -- added by loading config
   controls = {},
   updaters = {},
   long_updaters = {},
   on_enter = function(_) end,

   ui_helpers = {},
   navigation_ui_helpers = {},

   fuel_to_stop = function(s)
      -- no idea where this 20 factor comes from
      return utils.distance(s.status.dx, s.status.dy) *
         s.status.engine_strength * s.status.burn_rate / (s.status.mass * 20)
   end,

   scale = 3.5,

   print = editor.print,
   write = editor.write,

   host = host_fs_proxy.create("host_fs"),
   game = love.filesystem.exists("game") and host_fs_proxy.create("game"),

   rovers = {
      deploy = lume.fn(rovers.deploy, ship),
      recover = lume.fn(rovers.recover, ship),
      list = lume.fn(rovers.list, ship),
   },

   engine = {
      restart = function()
         mission.record_event(ship, "engine_restart")
         return editor.invisible
      end,
   },

   orbital_lock = function()
      ship.api.editor.print(body.toggle_lock(ship, ship.target and
                                                ship.target.name))
   end,

   -- deprecated:
   modes = editor.modes,
   mode = editor.mode,
   activate_mode = function(_, mode) editor.activate_mode(mode) end,
   read_line = function(_, p, cb) editor.activate_minibuffer(p, cb) end,
   draw_flight = function()
      local y = (love.graphics:getHeight() -
                    love.graphics.getFont():getHeight() * 2)
      love.graphics.print(ship.api.editor.last_line(), 20, y)
   end,
}

-- polywell requires a table where you can just lookup key values with []
-- so our logic of turning "key1.key2" keys into nested operations happens
-- in the ship.find and ship.set functions
setmetatable(ship.api, {
                __index = function(_, path) return ship.api:find(path, true) end,
                __newindex = function(_, path, contents)
                   ship.api:set(path, contents) end,
                __separator = ".",
})

return ship
