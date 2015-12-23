return {
   name="Passenger fare to Delta Pavonis",
   id="d1855bf9-504d-4c6b-9adf-83286279c572",
   destinations={"Packsi"},
   success_events={"bimen1"},
   credits=450,
   success_message="Thanks for the ride.",
   accept_function=function(ship)
      ship.visas["Terran"] = (ship.visas["Terran"] or 0) + 2
      ship.visas["Yueh"] = (ship.visas["Yueh"] or 0) + 1
   end,
}
