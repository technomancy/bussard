local t = require("lunatest")
local lume = require("lume")

local ship = require("ship")
local body = require("body")
local ssh = require("ship.ssh")

function test_range()
   ship:enter("Wolf 294", true, true)
   ship.x, ship.y = 999999, 999999
   ship.sandbox.ssh_connect("guest", "")
   t.assert_equal(false, ship.comm_connected)
end

function test_guest_login()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y, ship.target = solotogo.x, solotogo.y, solotogo
   ship.sandbox.ssh_connect("guest", "")
   t.assert_equal(solotogo.name, ship.comm_connected)
end

function test_failed_no_target()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y = solotogo.x, solotogo.y
   ship.sandbox.ssh_connect("guest", "")
   t.assert_equal(false, ship.comm_connected)
end

function test_rover_login()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y, ship.target = solotogo.x, solotogo.y, solotogo
   ship.rovers.simple, ship.rover_clearance.Solotogo = 1, true
   ship.api.rovers.deploy("simple")
   t.assert_equal(0, ship.rovers.simple)
   ship.target_number = #ship.bodies
   ship.target = ship.bodies[ship.target_number]
   ship.sandbox.ssh_connect("guest", "")
   t.assert_equal("Rover 1", ship.comm_connected)

   local out, _, env = {}, unpack(ssh.test_sessions["Rover 1"])
   env.sb.print = lume.fn(table.insert, out)
   ship.sandbox.ssh_send_line("16 + 32")
   body.update(ship.bodies, 1)
   t.assert_equal("48", out[1])
   t.assert_equal(1, #out)
end
