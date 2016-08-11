local t = require("lunatest")

local ship = require("ship")

function test_map()
   ship.sandbox.dofile("/spoilers/solutions/map")
   local chunk, err = ship.sandbox.loadstring("map()")
   if(err) then error(err) end
   chunk()
   local map_mode = ship.api.editor.mode()
   t.assert_equal("map", ship.api.editor.current_mode_name())
   map_mode.map["up"]()
   map_mode.map["up"]()
   map_mode.map["left"]()
   map_mode.map["escape"]()
   t.assert_not_equal("map", ship.api.editor.current_mode_name())
end
