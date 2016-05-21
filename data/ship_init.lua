return function(ship)
   ship.system_name = "L 668-21"
   ship.flag = "Katilay"
   ship.name = "Adahn"
   ship.api.docs = ship.api.docs or {}
   ship.api.docs.backup = { ["msg1_rot13"] =
         love.filesystem.read("data/docs/traxus-1.rot13")}
end
