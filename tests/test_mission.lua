local t = require("lunatest")

local ship = require("ship")
local body = require("body")
local ssh = require("ship.ssh")
local mail = require("mail")

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
   local p = move_to("Portal: " .. target)
   ship.portal_time = 0.00000001
   ship.sandbox.ssh_connect("guest", "")
   body.update(ship.bodies, 1) -- start
   body.update(ship.bodies, 1) -- pass first yield
   if(not fail_ok) then
      t.assert_equal(ship.system_name, target)
   end
end

local ssh_run = function(ship, target, command)
   move_to(target)
   ship.sandbox.ssh_connect("guest", "")
   t.assert_equal(target, ship.comm_connected)
   ship.sandbox.ssh_send_line(command)
   body.update(ship.bodies, 1)
   ssh.logout_all(ship)
end

function test_missions()
   t.assert_equal(lume.count(ship.events), 0)
   t.assert_equal(lume.count(ship.mail_delivered), 0)
   ship:enter("L 668-21", true)
   ssh_run(ship, "Mirduka Station", "upgrade buy life_support")
   t.assert_number(lume.find(ship.upgrade_names, "life_support"))
   portal("Tana")
   mail.reply(ship, "d6069254-4182-4f96-a145-df309a992798") -- passenger2
   t.assert_equal(lume.count(ship.active_missions), 1)
   t.assert_true(ship.mail_delivered["nari-a-01"])
   ssh_run(ship, "Tana Prime", "echo hi")
   ship:update(64)
   ship:update(64)
   t.assert_true(ship.mail_delivered["nari-a-02"])
   t.assert_true(ship.mail_delivered["nari-a-03"])

   mail.reply(ship, "6e1b94ec-c317-487b-a00b-d410ae6bd495") -- nari-a-03
   t.assert_true(ship.mail_delivered["6e1b94ec-c317-487b-a00b-d410ae6bd495"])
   mail.reply(ship, "84f7b207-08e0-4a54-af7c-d6f97aedc703") -- nari-a-04
   ssh_run(ship, "Tana Prime", "echo hi")
   mail.reply(ship, "c83c2439-f4cf-475f-95a6-f15cafc3db16") -- nari-a-05
   t.assert_true(ship.mail_delivered["c83c2439-f4cf-475f-95a6-f15cafc3db16"])

   portal("Luyten's Star")
   -- portal("Sol", true)
   ship:update(64)
   pp(ship.mail_delivered)
   -- t.assert_true(ship.mail_delivered["nari-a-07"])
end
