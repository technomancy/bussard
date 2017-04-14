local t = require("lunatest")
local ship = require("ship")
local body = require("body")

local function test_gravitate_all_moves_ship()
   local original_x, original_y = ship.x, ship.y
   body.gravitate_all(ship.bodies, ship, 1)
   t.assert_not_equal(lume.serialize({original_x, original_y}),
                      lume.serialize({ship.x, ship.y}))
end

local function test_engine_uses_fuel_and_charges()
   local down = love.keyboard.isDown
   love.keyboard.isDown = function(x) return(x == "up") end
   ship.fuel, ship.battery = 12, ship.battery_capacity / 2
   ship:update(1)
   t.assert_lt(12, ship.fuel)
   t.assert_gt(ship.battery_capacity / 2, ship.battery)
   love.keyboard.isDown = down
end

return {test_gravitate_all_moves_ship, test_engine_uses_fuel_and_charges}
