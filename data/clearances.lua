--- Table of from:to system names to checks

-- the key for each check pair is an event, and the value is the message to show
-- when you try to pass without that event having happened.

local no_terran = "Your ship is not authorized for entry into the Terran Republic."

local no_yueh = "Please see visit a Yueh embassy on a nearby world to obtain passage to Yueh."

local no_katilay = "The Katilay portal only operates at scheduled times; please visit the Katilay\noffice on Tirakir to apply for a permit to pass."
local katilay_damaged = "This portal is inoperable."

return {["Luyten's Star:Sol"] = {function(ship) return ship.humans.nari end =
      "Interportal requires background check for security."},

   ["Sol:Ross"] = {["find_dorath_accept"] = no_terran},
   ["Sol:Lalande"] = {["find_dorath_accept"] = no_terran},
   ["Lalande:Katilay"] = {[{}] = katilay_damaged},
   ["Mecalle:Katilay"] = {[{}] = no_katilay},
   ["Lalande:Yueh"] = {[{}] = no_yueh},
   ["Bohk:Yueh"] = {[{}] = no_yueh},
}
