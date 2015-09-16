local utils = require("utils")

local pages = {ual = "manual.md",
               keycodes = "keycodes.md",
}

return {
   -- need to have a string message so just `help' works vs help()
   message = "Type `man()` to view your ship's operating manual.",
   man = function(page)
      page = pages[page] or "manual.md"
      return utils.read_file(page)
   end
}
