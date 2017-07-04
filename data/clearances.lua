--- Table of from:to system names to array of checks

-- Checks can be either an event/reason pair or a function which returns a
-- reason string.

local systems = require("data.systems")

local no_fine = function(ship)
   if(ship.fine > 0) then
      return "You may not use any portals since you have " ..
         "outstanding fines from unauthorized portal use."
   end
end

local no_loan = function(ship)
   if(ship.loan > 0) then
      local system = systems[ship.system_name]
      return "You may not leave " .. system.gov .. " systems until your " ..
      "loan of " .. ship.loan .. " is paid off, which you can do by " ..
         "logging into any port and running: loan repay " .. ship.loan
   end
end

return {
   any = {no_fine},
   interportal = {no_loan},
}
