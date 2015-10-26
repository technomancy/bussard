local lume = require("lume")

local pages = {
   intro = "doc/intro.md",
   intro2 = "doc/intro2.md",
   intro3 = "doc/intro3.md",
   intro4 = "doc/intro4.md",
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
   man = function(ship, page_name)
      if(page_name == "list") then
         ship.repl.print(list())
      elseif(pages[page_name]) then
         -- can't inline this because read returns multiple values
         local p = love.filesystem.read(pages[page_name])
         ship.repl.print(p)
      elseif(page_name) then
         ship.repl.print("Page not found.\n" .. list())
      else
         local p = love.filesystem.read("doc/intro.md")
         ship.repl.print(p)
      end
   end
}
