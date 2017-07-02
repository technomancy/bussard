local go4_source = love.filesystem.read("data/host_src/gangof4.lua")
local go4 = {files={["/home/guest/bin/gangof4"] = go4_source}}
local trainee_rc = love.filesystem.read("data/host_src/trainee_rc.lua")

local data = { ["merdeka-station"] = { root = go4 },
   ["kembali-station"] = { root = go4 },
   ["kenapa-station"] = { root = go4 },
   ["earth"] = { root = go4 },
   ["trainee01"] = {guest = {files={["/home/guest/_smashrc"]=trainee_rc}}},
             }

return data
