-- for a new game
return function(ship)
   ship.system_name = "Sol"
   ship.flag = "Katilay"
   ship.name = "Adahn"
   ship.api.docs = ship.api.docs or {}
   ship.api.docs.mail = ship.api.docs.mail or
      { inbox = { _unread = {} },
        jobs = { _unread = {} },
        archive = { _unread = {} },
      }
   for _,v in pairs(love.filesystem.getDirectoryItems("data/src")) do
      ship.api.src[v] = ship.api.src[v] or love.filesystem.read("data/src/"..v)
   end
end
