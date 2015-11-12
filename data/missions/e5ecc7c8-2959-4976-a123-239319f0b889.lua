local utils = require("utils")
local orbit_time = 0

return {
   name="Test sensor on Wolf 294",
   id="e5ecc7c8-2959-4976-a123-239319f0b889",
   destinations={"Mars"},
   objectives={"new-phobos-flyby"},
   success_events={"jinod3"},
   credits=200,
   cargo={["sensor_array"]=2},
   success_message="Thanks; these results should prove helpful!",

   updater=function(ship)
      if(ship.system_name == "New Phobos" and
            not ship.events["new-phobos-flyby"] and
         utils.distance(ship.x, ship.y) < 3000) then
         orbit_time = orbit_time + dt
         if(orbit_time > 500) then
            ship.api.repl.print("Sensor readings complete.")
            ship.events["new-phobos-flyby"] = true
         end
      else
         orbit_time = 0
      end
   end,

   accept_function=function(ship)
      local deadline = utils.time(ship) + 20000
      ship.visas["Terran"] = deadline
      ship.visas["Bohk"] = deadline
      table.insert(ship.upgrade_names, "passponder")
      ship:recalculate()
   end,

   fail_function=function(ship)
      table.remove(ship.upgrade_names, "passponder")
      ship:recalculate()
   end,
}
