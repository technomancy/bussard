local t = require("lunatest")

local ship = require("ship")
local body = require("body")
local ssh = require("ship.ssh")
local mail = require("mail")
local mission = require("mission")

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

local portal = function(target, fail_ok)
   local p = move_to(target)
   ship.portal_time = 0.00000001
   ship.sandbox.ssh_connect("guest", "")
   body.update(ship.bodies, 1) -- start
   body.update(ship.bodies, 1) -- pass first yield
   if(not fail_ok) then
      local target_system = target:gsub("Inter", ""):gsub("[pP]ortal: ", "")
      t.assert_equal(target_system, ship.system_name)
   end
end

local ssh_run = function(ship, target, command)
   move_to(target)
   ship.sandbox.ssh_connect("guest", "")
   t.assert_equal(target, ship.comm_connected)
   ship.sandbox.ssh_send_line(command or "echo greetings")
   mission.update(ship, 1)
   body.update(ship.bodies, 1)
   ssh.logout_all(ship)
end

function test_missions()
   -- ACT 1
   t.assert_equal(lume.count(ship.events), 0)
   t.assert_equal(lume.count(ship.mail_delivered), 0)
   ship:enter("L 668-21", true)
   ssh_run(ship, "Mirduka Station", "upgrade buy life_support")
   t.assert_number(lume.find(ship.upgrade_names, "life_support"))
   portal("Portal: Tana")
   mail.reply(ship, "d6069254-4182-4f96-a145-df309a992798") -- passenger2
   t.assert_equal(lume.count(ship.active_missions), 1)
   t.assert_true(ship.mail_delivered["nari-a-01"])
   ssh_run(ship, "Tana Prime")
   ship:update(64)
   ship:update(64)
   t.assert_true(ship.mail_delivered["nari-a-02"])
   t.assert_true(ship.mail_delivered["nari-a-03"])

   mail.reply(ship, "6e1b94ec-c317-487b-a00b-d410ae6bd495") -- nari-a-03
   t.assert_true(ship.mail_delivered["6e1b94ec-c317-487b-a00b-d410ae6bd495"])
   mail.reply(ship, "84f7b207-08e0-4a54-af7c-d6f97aedc703") -- nari-a-04
   ssh_run(ship, "Tana Prime")
   t.assert_equal("companion", ship.humans.nari)
   mail.reply(ship, "c83c2439-f4cf-475f-95a6-f15cafc3db16") -- nari-a-05
   t.assert_true(ship.mail_delivered["c83c2439-f4cf-475f-95a6-f15cafc3db16"])

   portal("Portal: Luyten's Star")
   portal("Interportal: Sol")
   ssh_run(ship, "Newton Station")
   t.assert_true(ship.mail_delivered["nari-a-09"])
   t.assert_true(ship.events["passenger2"])

   -- ACT 2
   mail.reply(ship, "c369f9dc-9041-4672-8d0c-b7d28894e20d") -- nari-a-09
   t.assert_true(ship.events["rot13-decrypt-accept"])
end
