-- TODO: auto-generate passenger missions
return {
   name="passenger1",
   description="Passenger run from Solotogo to Tana Prime",
   id="4d6ce083-4d5e-4ca9-b58f-479ae6de27dc",
   destinations={"Solotogo", "Tana Prime"},
   destination_msgs={["Solotogo"] = "Serepiem Kacien has boarded.",
      ["Tana Prime"] = "Serepiem Kacien has disembarked.",
   }
   credits=250,
   success_events={"passenger1"},
   success_message="Thanks for the ride.",
   prereq=function(ship)
      if(require("lume").find(ship.upgrade_names, "life_support")) then
         return true
      else
         return false, "Cannot take passengers without onboard life support sytem."
      end
   end
}
