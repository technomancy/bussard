local go4_source = love.filesystem.read("data/host_src/gangof4.lua")
local go4 = {files={["/home/guest/bin/gangof4"] = go4_source}}
local trainee_rc = love.filesystem.read("data/host_src/trainee_rc.lua")

local maze_files = {
   ["/bin/door"] = love.filesystem.read("data/host_src/door.lua")
}

local data = { ["merdeka-station"] = { root = go4 },
   ["kembali-station"] = { root = go4 },
   ["telemoyo-station"] = { root = go4 },
   ["earth"] = { root = go4 },
   ["trainee01"] = {guest={files={["/home/guest/_smashrc"]=trainee_rc}},
                    root={files=maze_files}},
             }

return data
