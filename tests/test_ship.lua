local t = require("lunatest")

local ship = require("ship")

function test_ship_find()
   t.assert_table(ship.api:find("src."))
end
