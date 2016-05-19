local lume = require("lume")

local upgrades = {"laser", "engine", "cargo_bay", "fuel_tank", "fuel_charger",
                  "solar_panel", "comm_boost", "map",}

local pages = {
   manual = "manual.md",
   keycodes = "doc/keycodes.md",
   quickstart = "doc/quickstart.md",
   quickstart2 = "doc/quickstart2.md",
   quickstart3 = "doc/quickstart3.md",
   tutorial = "doc/quickstart.md", -- aliases for quickstart
   help = "doc/quickstart.md",
   intro = "doc/quickstart.md",

   laser = "doc/laser.md",
   engine = "doc/engine.md",
   cargo_bay = "doc/cargo_bay.md",
   fuel_tank = "doc/fuel_tank.md",
   fuel_charger = "doc/fuel_charger.md",
   solar_panel = "doc/solar_panel.md",
   comm_boost = "doc/comm_boost.md",
   life_support = "doc/life_support.md",
   map = "doc/map.md",
}

local show_page = function(ship, p)
   return not lume.find(upgrades, p) or lume.find(ship.status.upgrade_names, p)
end

local list = function(ship)
   local have_pages = lume.filter(lume.keys(pages), lume.fn(show_page, ship))
   return "Manual pages:\n* " .. table.concat(lume.sort(have_pages), "\n* ")
end

return {
   -- need to have a string message so just `help' works vs help()
   message = "Type `man()` to view your ship's quickstart guide.",
   man = function(ship, page_name)
      if(page_name == "list") then
         ship.print(list(ship))
      elseif(pages[page_name] and show_page(ship, page_name)) then
         -- can't inline this because read returns multiple values
         local p = love.filesystem.read(pages[page_name])
         ship.print(p)
      elseif(page_name) then
         ship.print("Page not found.\n\n" .. list())
      else
         local p = love.filesystem.read("doc/quickstart.md")
         ship.print(p)
      end

      return ship.editor.invisible
   end,

   get = function(page_name)
      local p = love.filesystem.read(pages[page_name])
      return p
   end
}
