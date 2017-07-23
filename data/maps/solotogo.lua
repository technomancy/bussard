local lume = require("lume")
local fs = require("love.filesystem")

local map = lume.array(fs.lines("data/maps/solotogo.map"))

map.motd = "This isn't finished yet, sorry!"
map.triggers = {
   ["42x207"] = function() record_event("memory_card_delivered") end,
}

return map
