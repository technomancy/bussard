local t = require("lunatest")

local ship = require("ship")

local function test_ship_find()
   t.assert_table(ship.api:find("src."))
end

return {test_ship_find=test_ship_find}
