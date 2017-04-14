local t = require("lunatest")
local lume = require("lume")

local ship = require("ship")
local body = require("body")

local exec = function(command)
   local send, recv = ship.sandbox.ssh_connect("guest", "")
   assert(send, "Could not connect")
   recv(true) recv(true) -- discard motd, set_prompt
   send(command)
   local val = recv(true)
   send("logout")
   recv(true) recv(true) -- discard set_prompt, etc
   return val
end

local function test_loans()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y, ship.target = solotogo.x, solotogo.y, solotogo
   ship.target_number, ship.target = 2, solotogo
   ship.credits, ship.loan = 128, 0

   -- initial borrow
   exec("loan borrow 128")
   t.assert_equal(256, ship.credits)
   t.assert_equal(154, ship.loan)

   -- attempting to exceed credit limit
   exec("loan borrow 10000")
   t.assert_equal(256, ship.credits)
   t.assert_equal(154, ship.loan)

   -- repaying
   exec("loan repay 154")
   t.assert_equal(102, ship.credits)
   t.assert_equal(0, ship.loan)
end

return {test_loans=test_loans}
