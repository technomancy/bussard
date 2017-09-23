local t = require("lunatest")

local ship = require("ship")
local body = require("body")
local mail = require("mail")
local mission = require("mission")
local client = require("os.client")

local d = lume.fn(ship.api.editor.with_current_buffer, "*console*",
                  ship.api.editor.debug)

local get_body = function(name)
   for _,b in ipairs(ship.bodies) do
      if(b.name == name) then
         return b
      end
   end
end

local move_to = function(target)
   local b = get_body(target) or error("Target not found: " .. target)
   ship.x, ship.y, ship.dx, ship.dy, ship.target = b.x, b.y, b.dx, b.dy, b
end

local ssh_run = function(ship, target, command, username, password)
   move_to(target)
   local send = ship.sandbox.ssh_connect(username or "guest", password or "")
   t.assert_function(send)
   send(command)
   mission.update(ship, 1)
   love.timer.sleep(0.8)
   client.update(ship, true)
   send("logout")
end

local function test_missions()
   -- ACT 1
   ship:enter("L 668-21", true)
   t.assert_false(ship.mail_delivered["dex19-2"])

   ship.api.engine.restart()
   ship:update(64)
   t.assert_true(ship.mail_delivered["dex19-2"])

   ship:update(64)
   t.assert_false(pcall(portal, "Portal: Tana"))

   move_to("Merdeka Station")
   ship:update(64)
   t.assert_true(ship.mail_delivered["dex19-3"])

   ssh_run(ship, "Merdeka Station",
           "f l f f r f f r f f login ",
           "trainee", "reindeerflotilla")
   ship:update(64)
   mission.update(ship, 1)

   t.assert_number(ship.events["trainee01"])
   t.assert_true(ship.credits > 512)
   t.assert_true(ship.mail_delivered["dex19-4"])
   t.assert_true(ship.mail_delivered["cmec-recruit"])

   ssh_run(ship, "Merdeka Station", "upgrade buy battery")
   t.assert_number(lume.find(ship.upgrade_names, "battery"))
end

return {}-- {test_missions=test_missions}
