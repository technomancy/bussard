local utils = require("utils")
local lume = require("lume")

local default_config = love.filesystem.read("data/src/config")

local help = require("ship.help")
local upgrade = require("ship.upgrade")
local ai = require("ship.ai")
local ssh = require("ship.ssh")

local mail = require("mail")
local mission = require("mission")

-- for shuffling systems upon entry
local asteroid = require("asteroid")
local body = require("body")

local editor = require("ship.editor")

local scale_min = 1

local status_whitelist = {
   "x", "y", "dx", "dy", "heading", "target", "system_name", "bodies",
   "fuel", "fuel_capacity", "battery", "battery_capacity", "mass", "in_range",
   "engine_on", "turning_right", "turning_left", "credits", "upgrade_names",
   "cargo", "cargo_capacity", "solar",
   "engine_strength", "turning_speed", "cpuinfo",
   "recharge_rate", "burn_rate", "comm_connected", "comm_range", "scoop_range",
   "portal_range", "portal_time", "flag", "target", "comm_boost",
}

local base_stats = {
   mass = 128,
   cargo_capacity = 64,
   fuel_capacity = 192,
   scoop_range = 0,
   comm_range = 2048,
   recharge_rate = 4,
   burn_rate = 16,
   engine_strength = 1024,
   turning_speed = 4,
   battery_capacity = 128,
   solar = 30,

   portal_range = 1024,
   portal_time = 4000, -- in-game seconds
}

local function find_binding(ship, key, the_mode)
   local mode = the_mode or ship.api:mode()
   local ctrl = love.keyboard.isDown("lctrl", "rctrl", "capslock")
   local alt = love.keyboard.isDown("lalt", "ralt")
   local map = (ctrl and alt and mode["ctrl-alt"]) or
      (ctrl and mode.ctrl) or (alt and mode.alt) or mode.map

   return map[key] or map["__any"] or
      (mode.parent and find_binding(ship, key, mode.parent))
end

local define_mode = function(ship, name, textinput, wrap, activate)
   ship.api.modes[name] = { map = {}, ctrl = {}, alt = {}, ["ctrl-alt"] = {},
                            wrap = wrap, textinput = textinput,
                            activate = activate, name = name }
end

local bind = function(ship, mode, keycode, fn)
   assert(keycode ~= nil, "Tried to bind to nil. Use false to unbind")
   if(type(mode) == "table") then
      for _,m in ipairs(mode) do
         ship.sandbox.bind(m, keycode, fn)
      end
   else
      -- lua regexes don't support |
      local map, key = keycode:match("(ctrl-alt)-(.+)")
      if not map then map, key = keycode:match("(ctrl)-(.+)") end
      if not map then map, key = keycode:match("(alt)-(.+)") end
      if map == "alt-ctrl" then map = "ctrl-alt" end
      assert(ship.api.modes[mode], "No mode " .. mode)
      ship.api.modes[mode][map or "map"][key or keycode] = fn
   end
end

local sandbox_loadstring = function(ship, code)
   local chunk, err = loadstring(code)
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
   return sandbox_loadstring(ship, contents)()
end

local sandbox_loaded = {}

local sandbox_require = function(ship, mod_name)
   if(sandbox_loaded[mod_name]) then return end
   sandbox_dofile(ship, mod_name)
   sandbox_loaded[mod_name] = true
end

local sandbox = function(ship)
   return lume.merge(utils.sandbox,
                     {  help = help.message,
                        default_config = default_config,
                        print = ship.api.print,
                        realprint = print,
                        -- clear = ship.editor.clear_lines,
                        ship = ship.api,
                        _LOADED = sandbox_loaded,
                        dofile = lume.fn(sandbox_dofile, ship),
                        require = lume.fn(sandbox_require, ship),
                        loadstring = lume.fn(sandbox_loadstring, ship),
                        debug = {traceback = debug.traceback},
                        os = {time = lume.fn(utils.time, ship)},
                        -- scp = lume.fn(comm.scp, ship),
                        man = lume.fn(help.man, ship.api),
                        define_mode = lume.fn(define_mode, ship),
                        bind = lume.fn(bind, ship),
                        ssh_connect = lume.fn(ssh.connect, ship),
                        ssh_send_line = lume.fn(ssh.send_line, ship),
   })
end

local target_dt = 0.03 -- about 33 frames per second

local trajectory_auto = function(ship, dt)
   if(not ship.trajectory_adjust_progress) then
      ship.trajectory_adjust_progress = 5
      return
   end

   ship.trajectory_adjust_progress = ship.trajectory_adjust_progress - dt

   if(ship.trajectory_adjust_progress <= 0) then
      ship.updaters.trajectory_auto = nil
      ship.trajectory_adjust_progress = nil
   elseif(ship.trajectory_adjust_progress < 4) then
      return -- give it a second to stabilize, then reduce
   elseif(dt > target_dt * 1.3) then
      ship.trajectory = ship.trajectory * 0.8
      ship.trajectory_step_size = ship.trajectory_seconds / ship.trajectory
   elseif(dt < target_dt * 0.7) then
      ship.trajectory = ship.trajectory * 1.2
      ship.trajectory_step_size = ship.trajectory_seconds / ship.trajectory
   end
end

local ship = {
   base_stats = base_stats,

   -- ephemeral
   x=0, y=0, dx=0, dy=0, heading = math.pi,
   engine_on = false,
   turning_right = false,
   turning_left = false,
   comm_connected = false,
   target_number = 0,
   target = nil,
   mass = 128,
   battery = 128,
   upgrades = {},
   time_factor = 1000,

   -- keep around
   fuel = 128,
   credits = 1024,
   time_offset = utils.game_start,
   system_name = "L 668-21",
   cargo = {["food"] = 2},
   upgrade_names = {},
   active_missions={},
   events = {},
   flag = "Tana",
   name = "Adahn",

   cpuinfo = {processors=64, arch="arm128-ng", mhz=2800},
   configure = function(ship, systems, ui)
      for _,m in pairs(ship.api.modes) do
         if(m.initialize) then m.initialize() end
      end

      ship.api.ui = ui
      ship.systems = systems

      ship.sandbox = sandbox(ship)
   end,

   dofile = sandbox_dofile,

   enter = function(ship, system_name, reseed, suppress_message)
      local from = ship.system_name
      assert(ship.systems[system_name], system_name .. " not found.")
      if(not suppress_message) then
         ship.api.editor.print("Entering the " .. system_name .. " system.")
      end

      -- stuff these things in there to expose to in-ship APIs
      ship.bodies = ship.systems[system_name].bodies
      ship.system_name = system_name

      ssh.logout_all(ship)
      ship:recalculate()

      if(reseed) then
         ship.engine_on, ship.turning_right, ship.turning_left = false,false,false
         ship.comm_connected, ship.target_number, ship.target = false, 0, nil

         -- re-seed system-level things
         asteroid.populate(ship.systems[ship.system_name])
         for _,b in pairs(ship.bodies) do
            body.seed_pos(b, ship.bodies[1])
            body.seed_cargo(b)
         end

         local portal = lume.match(ship.bodies, function(b) return b.portal == from end)
         if(portal) then
            ship.x, ship.y = portal.x + 100, portal.y + 100
            ship.dx, ship.dy = portal.dx, portal.dy
         else
            ship.x = love.math.random(30000) + 10000
            ship.y = love.math.random(30000) + 10000
         end
         ai.seed(system_name, ship.bodies)
         mail.deliver(ship, system_name)
      end
   end,

   update = function(ship, dt)
      ship.api.dt = dt

      -- activate controls
      local current_mode = ship.api:mode()
      if(current_mode and current_mode.name == "flight") then
         for k,f in pairs(ship.api.controls) do
            f(love.keyboard.isDown(k))
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
         -- TODO: move to an upgrade
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

      for _,f in pairs(ship.api.updaters or {}) do
         f(ship.api, dt)
      end

      for _,u in pairs(ship.upgrades) do
         if(u.update) then u.update(ship, dt) end
      end

      mission.update(ship, dt)
      ship:enforce_limits()
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
      if(ship.api.trajectory_auto) then
         ship.api.updaters.trajectory_auto = trajectory_auto
      end

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

      -- TODO: this needs to be declarative
      if(ship.events["jinod3"]) then
         for _,b in ipairs(ship.systems["Sol"].bodies) do
            if(b.name == "Mars") then
               if(not lume.find(b.upgrades, "comm_boost")) then
                  table.insert(b.upgrades, "comm_boost")
                  body.seed_cargo(b, true)
               end
            end
         end
      end
   end,

   cargo_mass = function(ship)
      local m = 0
      for _,c in pairs(ship.cargo) do m = m + c end
      return m
   end,

   enforce_limits = function(ship)
      if(ship.api.scale < scale_min) then ship.api.scale = scale_min end
   end,

   -- interface
   handle_key = function(ship, key, ...)
      local fn = find_binding(ship, key)
      local wrap = ship.api:mode().wrap
      if(fn and wrap) then wrap(fn, ...)
      elseif(fn) then fn(...)
      end
   end,

   textinput = function(ship, text, the_mode)
      if(find_binding(ship, text)) then return end
      if(text:len() > 1) then return end
      local mode = the_mode or ship.api:mode()
      if(mode.textinput) then
         if(mode.wrap) then
            mode.wrap(mode.textinput, text)
         else
            mode.textinput(text)
         end
      elseif(mode.parent) then
         ship:textinput(text, mode.parent)
      end
   end,
}

-- everything in here is exposed to the sandbox. this table *is* `ship`, as far
-- as the in-game code is concerned.
ship.api = {
   editor = editor,
   help = help,

   modes = { minibuffer = { map = { ["return"] = editor.exit_minibuffer,
                               escape = lume.fn(editor.exit_minibuffer, true),
                               backspace = editor.delete_backwards, },
                            ctrl = {g=lume.fn(editor.exit_minibuffer, true),},
                            alt = {}, ["ctrl-alt"] = {},
                            wrap = editor.wrap, textinput = editor.textinput,
                            name = "minibuffer",
           }},

   mode = function(s)
      return s.modes[s.editor.current_mode_name() or "flight"]
   end,

   activate_mode = function(s, mode_name)
      if(s:mode().deactivate) then
         s:mode().deactivate()
      end
      s.editor.set_mode(mode_name)
      if(s.modes[mode_name].activate) then
         s.modes[mode_name].activate()
      end
   end,

   mission = {
      list = lume.fn(mission.list, ship),
      abort = lume.fn(mission.abort, ship),
      hud = {x=-300, y=-100, type="text", format="Missions: %s",
             values = {lume.fn(mission.readout, ship)}}
   },
   -- data tables (read-only)
   sensors = utils.whitelist_table(ship, status_whitelist, "sensors"),
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
         ship.target = ship.bodies[ship.target_number]
      end,
      closest_target = function()
         local min_distance = 1000000000000
         for i,b in ipairs(ship.bodies) do
            if(utils.distance(ship, b) < min_distance) then
               ship.target_number = i
               min_distance = utils.distance(ship, b)
            end
         end
         ship.target = ship.bodies[ship.target_number]
      end,
   },

   find = function(s, path)
      local parts = lume.split(path, ".")
      local target = s
      for _,p in ipairs(parts) do
         if(type(target) == "table") then
            target = target[p]
         else
            return nil
         end
      end
      return target
   end,

   dofile = lume.fn(sandbox_dofile, ship),

   -- for user files
   src = {},
   docs = {},
   persist = {"persist", "scale", "src", "docs", "trajectory_seconds",
              "trajectory", "trajectory_step_size", "trajectory_auto"},

   -- added by loading config
   controls = {},
   commands = {},
   updaters = {},
   helm = love.keyboard,

   -- trajectory plotting is the single biggest perf drain by far
   -- these numbers will be changed if the frame rate is too low
   trajectory = 256,
   trajectory_step_size = 0.1,
   trajectory_seconds = 15, -- how far out the trajectory should go
   trajectory_auto = true, -- turn this off to disable auto-adjustment

   fuel_to_stop = function(s)
      -- no idea where this 20 factor comes from
      return utils.distance(s.status.dx, s.status.dy) *
         s.status.engine_strength * s.status.burn_rate / (s.status.mass * 20)
   end,

   scale = 1.9,

   cheat = ship,
   print = editor.print,
   write = editor.write,

   read_line = function(_, prompt, callback)
      editor.activate_minibuffer(prompt, callback)
   end,
}

return ship
