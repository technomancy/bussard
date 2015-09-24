local lume = require("lume")

local pages = {
   manual = "manual.md",
   keycodes = "doc/keycodes.md",
   laser = "doc/laser.md"
}

local list = function()
   return "Manual pages:\n* " .. table.concat(lume.keys(pages), "\n* ")
end

return {
   -- need to have a string message so just `help' works vs help()
   message = "Type `man()` to view your ship's operating manual.",
   man = function(page_name)
      if(page_name == "list") then
         return list()
      elseif(pages[page_name]) then
         local p = love.filesystem.read(pages[page_name])
         return p
      elseif(page_name) then
         return "Page not found.\n" .. list()
      else
         local p = love.filesystem.read("manual.md")
         return p
      end
   end
}
