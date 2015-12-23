local lume = require("lume")

return { restrictions={"bimen1"},
         chance = 25,
         gov={"Sol"},
         prereq_fn = function(ship)
            return lume.find(ship.upgrade_names, "passponder")
         end,
         mission_id="d1855bf9-504d-4c6b-9adf-83286279c572",
}
