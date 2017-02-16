return {
   name="passenger3",
   description="Passenger run from Kembali Station to Merdeka Station",
   id="bdee5736-f633-4c77-b97f-39d00e0577b4",
   destinations={"Kembali Station", "Merdeka Station"},
   destination_msgs={["Kembali Station"] = "Merous Kiebeb has boarded.",
   ["Merdeka Station"] = "Merous Kiebeb has disembarked."},
   credits=325,
   success_events={"passenger3"},

   check_prereq=function(ship)
      if(require("lume").find(ship.upgrade_names, "life_support")) then
         return true
      else
         return false, "Cannot take passengers without onboard life support sytem."
      end
   end,
   on_accept = function(ship)
      ship.humans["Merous Kiebeb"] = "bdee5736-f633-4c77-b97f-39d00e0577b4"
   end,
}
