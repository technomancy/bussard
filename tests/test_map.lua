local t = require("lunatest")

local ship = require("ship")
local editor = ship.api.editor

function test_map()
   ship.sandbox.dofile("/spoilers/solutions/map")
   local chunk, err = ship.sandbox.loadstring("map()")
   if(err) then error(err) end
   chunk()
   t.assert_equal("map", editor.current_mode_name())
   editor.keypressed("up")
   editor.keypressed("up")
   editor.keypressed("left")
   editor.keypressed("escape")
   t.assert_not_equal("map", editor.current_mode_name())
end
