local t = require("lunatest")

local ship = require("ship")
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

function test_failed_login()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y = solotogo.x, solotogo.y
   ship.sandbox.ssh_connect("guest", "")
   t.assert_equal(false, ship.comm_connected)
end
