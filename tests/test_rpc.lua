local t = require("lunatest")
local lume = require("lume")

local ship = require("ship")
local body = require("body")

function test_loans()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y, ship.target = solotogo.x, solotogo.y, solotogo
   ship.target_number, ship.target = 2, solotogo
   local send = ship.sandbox.ssh_connect("guest", "")
   t.assert_function(send)

   ship.credits, ship.loan = 128, 0

   -- initial borrow
   ship.sandbox.ssh_send_line("loan borrow 128")
   body.update(ship.bodies, 1)
   t.assert_equal(256, ship.credits)
   t.assert_equal(154, ship.loan)

   -- attempting to exceed credit limit
   ship.sandbox.ssh_send_line("loan borrow 10000")
   body.update(ship.bodies, 1)
   t.assert_equal(256, ship.credits)
   t.assert_equal(154, ship.loan)

   -- repaying
   ship.sandbox.ssh_send_line("loan repay 154")
   body.update(ship.bodies, 1)
   t.assert_equal(102, ship.credits)
   t.assert_equal(0, ship.loan)
end
