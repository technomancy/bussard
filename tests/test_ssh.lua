local t = require("lunatest")
local lume = require("lume")

local ship = require("ship")
local body = require("body")

local function test_range()
   ship:enter("Wolf 294", true, true)
   ship.x, ship.y = 999999, 999999
   local send = ship.sandbox.ssh_connect("guest", "")
   t.assert_true(not send)
end

local function test_guest_login()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y, ship.target = solotogo.x, solotogo.y, solotogo
   local send = ship.sandbox.ssh_connect("guest", "")
   t.assert_function(send)
end

local function test_failed_no_target()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y = solotogo.x, solotogo.y
   local send = ship.sandbox.ssh_connect("guest", "")
   t.assert_true(not send)
end

local function test_failed_bad_creds()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y, ship.target = solotogo.x, solotogo.y, solotogo
   local send = ship.sandbox.ssh_connect("guest", "12345")
   t.assert_true(not send)
end

local function test_rover_login()
   ship:enter("Wolf 294", true, true)
   local solotogo = ship.bodies[2]
   ship.x, ship.y, ship.target = solotogo.x, solotogo.y, solotogo
   local send = ship.sandbox.ssh_connect("nari02", "mendaciouspolyglottal")
   -- t.assert_equal("function", type(send))

   -- send("16 32 + .s")
   -- t.assert_equal("48", ship.api.editor.get_line(-1))
end

return {test_range = test_range, test_guest_login = test_guest_login,
        test_failed_no_target = test_failed_no_target,
        test_failed_bad_creds = test_failed_bad_creds,
        test_rover_login = test_rover_login}
