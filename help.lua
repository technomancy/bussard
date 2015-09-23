local utils = require("utils")

local pages = {ual = "manual.md",
               keycodes = "doc/keycodes.md",
               laser = "doc/laser.md"
}

return {
   -- need to have a string message so just `help' works vs help()
   message = "Type `man()` to view your ship's operating manual.",
   man = function(page)
      page = pages[page] or "manual.md"
      return love.filesystem.read(page)
   end
}
