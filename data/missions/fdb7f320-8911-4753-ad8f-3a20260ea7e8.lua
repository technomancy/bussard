local utils = require("utils")
local orbit_time = 0

return {
   name="Test sensor on Wolf 294",
   id="fdb7f320-8911-4753-ad8f-3a20260ea7e8",
   destinations={"Mars"},
   objectives={"wolf-294-flyby"},
   success_events={"jinod2"},
   credits=200,
   cargo={["sensor_array"]=2},
   success_message="Thanks; we'll analyze these results immediately!",

   updater=function(ship, dt)
      if(ship.system_name == "Wolf 294" and
            not ship.events["wolf-294-flyby"] and
         utils.distance(ship.x, ship.y) < 5000) then
         orbit_time = orbit_time + (dt * ship.time_factor)
         if(orbit_time > 250) then
            ship.api.repl.print("Sensor readings complete.")
            ship.events["wolf-294-flyby"] = true
         end
      else
         orbit_time = 0
      end
   end,
}
