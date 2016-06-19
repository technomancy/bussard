local lume = require("lume")

return function(ship, mission_id)
   local human = mission_id:gsub("recover_", "")
   local destination = ship.humans_left_at[human]
   assert(destination, "No destination for recovering " .. human)
   return {
      name="Recover " .. human .. " at " .. destination,
      description="Recover " .. human .. " at " .. destination,
      id=mission_id,
      success_message=human .. ": All right, I'm back on board.",
      success_check=function()
         if(lume.find(ship.upgrade_names, "life_support")) then
            ship.humans[human] = "companion"
            return true
         end
      end
   }
end
