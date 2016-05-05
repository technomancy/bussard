return {
   name="passenger3",
   description="Passenger run from Apkabar Station to Mirduka Station",
   id="bdee5736-f633-4c77-b97f-39d00e0577b4",
   destinations={"Apkabar Station", "Mirduka Station"},
   credits=325,
   success_events={"passenger3"},

   prereq=function(ship)
      if(require("lume").find(ship.upgrade_names, "life_support")) then
         return true
      else
         return false, "Cannot take passengers without onboard life support sytem."
      end
   end
}
