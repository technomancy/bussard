local utils = require("utils")
local lume = require("lume")
local serpent = require("serpent")

local help = require("doc")
local upgrade = require("data.upgrades")
local ai = require("ship.ai")
local ssh = require("ship.ssh")
local mail = require("mail")
local mission = require("mission")
local host_fs_proxy = require("host_fs_proxy")

-- for shuffling systems upon entry
local asteroid = require("asteroid")
local body = require("body")

local editor = require("ship.editor")

local with_traceback = lume.fn(utils.with_traceback, editor.print)

local scale_min = 2

local status_whitelist = {
   "x", "y", "dx", "dy", "heading", "target", "system_name", "bodies",
   "fuel", "fuel_capacity", "battery", "battery_capacity", "mass",
   "engine_on", "turning_right", "turning_left", "credits", "upgrade_names",
   "cargo", "cargo_capacity", "solar",
   "engine_strength", "turning_speed", "cpuinfo",
   "recharge_rate", "burn_rate", "comm_connected", "comm_range", "scoop_range",
   "portal_range", "portal_time", "flag", "target", "comm_boost",
   "time_factor"
}

local base_stats = {
   mass = 128,
   cargo_capacity = 64,
   fuel_capacity = 192,
   scoop_range = 0,
   comm_range = 2048,
   recharge_rate = 1/2,
   burn_rate = 1,
   engine_strength = 512,
   turning_speed = 1/2,
   battery_capacity = 128,
   solar = 30,

   portal_range = 1024,
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

local universe_api = utils.readonly_proxy {
   g = body.g,
   max_accel = body.max_accel,
}

local sandbox = function(ship)
   local serpent_opts = {maxlevel=8,maxnum=64,nocode=true}
   -- TODO: document these in the in-game manual
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
                       ssh_connect = lume.fn(ssh.connect, ship),
                       ssh_send_line = lume.fn(ssh.send_line, ship),
                       ssh_get_connection = lume.fn(ssh.get_connection, ship),
                       reply = lume.fn(mail.reply, ship),
                       replyable = mail.replyable,
                       graphics = love.graphics,
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
                       end,})
end

local target_dt = 1/33

local trajectory_auto = function(ship, dt)
   if(dt > target_dt * 1.3 * ship.status.time_factor) then
      ship.trajectory = ship.trajectory * 0.8
      ship.trajectory_step_size = ship.trajectory_seconds / ship.trajectory
   elseif(dt < target_dt * 0.7 * ship.status.time_factor) then
      ship.trajectory = ship.trajectory * 1.2
      ship.trajectory_step_size = ship.trajectory_seconds / ship.trajectory
   else -- if we've stabilized our FPS sufficiently, disable adjustment
      ship.updaters.trajectory_auto = nil
   end
end

local ship = {
   base_stats = base_stats,

   -- ephemeral
   x=0, y=0, dx=0, dy=-200, heading = math.pi,
   engine_on = false,
   turning_right = false,
   turning_left = false,
   comm_connected = false,
   target_number = 0,
   target = nil,
   mass = 128,
   battery = 128,
   upgrades = {},
   time_factor = 10,

   -- keep around
   fuel = 128,
   credits = 1024,
   time_offset = utils.game_start,
   cargo = {},
   upgrade_names={},
   active_missions={},
   mail_delivered={},
   events={},
   humans={},
   humans_left_at={},

   cpuinfo = {processors=64, arch="arm128-ng", mhz=2800},
   configure = function(ship, systems, ui)
      ship.api.ui = ui
      ship.systems = systems

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
      local from = ship.system_name
      assert(ship.systems[system_name], system_name .. " not found.")
      if(not suppress_message) then
         ship.api.editor.print("Entering the " .. system_name .. " system.")
      end

      if(ship.api.trajectory_auto) then
         ship.api.updaters.trajectory_auto = trajectory_auto
      end

      -- stuff these things in there to expose to in-ship APIs
      ship.bodies = ship.systems[system_name].bodies
      ship.system_name = system_name

      ssh.logout_all(ship)
      ship:recalculate()

      if(reseed) then
         ship.comm_connected, ship.target_number, ship.target = false, 0, nil

         -- re-seed system-level things
         asteroid.populate(ship.systems[ship.system_name])
         for _,b in pairs(ship.bodies) do
            body.seed_pos(b, ship.bodies[1])
            body.seed_cargo(b)
         end

         local portal = lume.match(ship.bodies,
                                   function(b) return b.portal == from end)
         if(portal) then
            ship.x, ship.y = portal.x + 100, portal.y + 100
            ship.dx, ship.dy = portal.dx, portal.dy
         else
            ship.x = love.math.random(30000) + 10000
            ship.y = love.math.random(30000) + 10000
         end
         ai.seed(ship)
      end
      ship.api.on_enter(system_name)
   end,

   update = function(ship, dt)
      ship.api.dt = dt

      -- activate controls
      if(editor.current_mode_name() == "flight") then
         for k,f in pairs(ship.api.controls) do
            with_traceback(f, love.keyboard.isDown(k), dt)
         end
      end

      -- this seems overcomplicated at first glance--why are we
      -- setting an engine_on bit in the ship.controls handler and
      -- then checking it later? the answer is that otherwise it would
      -- be a sandbox violation. if the ship.controls handler affected
      -- acceleration directly, you could write code that would make
      -- the engine arbitrarily powerful or use zero fuel or
      -- whatever. so these two steps must remain separate.
      if(ship.engine_on and ship.fuel > 0) then
         local fx = (math.sin(ship.heading) * dt * ship.engine_strength)
         local fy = (math.cos(ship.heading) * dt * ship.engine_strength)
         ship.dx = ship.dx + fx / ship.mass
         ship.dy = ship.dy + fy / ship.mass
         ship.fuel = ship.fuel - (ship.burn_rate * dt)
      elseif(ship.fuel < ship.fuel_capacity) then
         ship.fuel = ship.fuel + (ship.recharge_rate * dt)
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

      utils.run_handlers(ship.api, "updaters", "broken_updaters", {ship.api, dt}, editor.print)

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
         ship.humans_left_at[human] = assert(ship.comm_connected,
                                             "Can't disembark nowhere.")
         mission.accept(ship, "recover_" .. human)
         mail.deliver_msg(ship, "recover_" .. human .. ".msg")
      elseif(m) then
         mission.fail(ship, m, false)
      end
   end,

   -- for debugging during development: ship.cheat:realdofile("main.lua")
   realdofile = function(ship, x) dofile(x) ship.api.ui.play() end,
   love = love,
}

-- everything in here is exposed to the sandbox. this table *is* `ship`, as far
-- as the in-game code is concerned.
ship.api = {
   editor = editor,
   help = help,

   mission = {
      list = lume.fn(mission.list, ship),
      abort = lume.fn(mission.abort, ship),
      hud = {x=-300, y=-100, type="text", format="Missions: %s",
             values = {lume.fn(mission.readout, ship)}}
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
                  (table.length(ship.bodies) + 1))
         else
            ship.target_number = ((ship.target_number + 1) %
                  (table.length(ship.bodies) + 1))
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
   },

   find = function(s, path)
      if(path == ".") then return s end
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
            if(type(target) == "table" and p ~= "") then
               target = target[p]
            elseif(p ~= "") then
               return nil
            end
         end
         return target
      end
   end,

   dofile = lume.fn(sandbox_dofile, ship),

   -- for user files
   src = {},
   docs = {},
   persist = {"persist", "scale", "src", "docs", "trajectory_seconds",
              "trajectory", "trajectory_step_size", "trajectory_auto"},
   persist_buffers = {"*console*"},

   -- added by loading config
   controls = {},
   updaters = {},
   long_updaters = {},
   on_enter = function(_) end,

   ui_helpers = {},
   navigation_ui_helpers = {},

   -- trajectory plotting is the single biggest perf drain by far
   -- these numbers will be changed if the frame rate is too low
   trajectory = 256,
   trajectory_step_size = 0.1,
   trajectory_seconds = 128, -- how far out the trajectory should go
   trajectory_auto = true, -- turn this off to disable auto-adjustment

   fuel_to_stop = function(s)
      -- no idea where this 20 factor comes from
      return utils.distance(s.status.dx, s.status.dy) *
         s.status.engine_strength * s.status.burn_rate / (s.status.mass * 20)
   end,

   scale = 3.5,

   cheat = ship,
   print = editor.print,
   write = editor.write,

   host = host_fs_proxy.create("host_fs"),
   game = love.filesystem.exists("game") and host_fs_proxy.create("game"),

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

return ship
