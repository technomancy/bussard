local lume = require("lume")

local upgrades = {"engine", "cargo_bay", "fuel_tank", "fuel_charger",
                  "solar_panel", "comm_boost", "map", "jeejah", "life_support",
                  "bencode_decoder", "underclocker", }

local events = { luabook1 = "passenger2", luabook2 = "passenger2",
                 luabook3 = "passenger2", luabook4 = "luabook-b",
                 luabook5 = "luabook-b", luabook6 = "luabook-b",
}

local pages = {
   manual = "manual.md",
   keycodes = "doc/keycodes.md",
   quickstart = "doc/quickstart.md",
   quickstart2 = "doc/quickstart2.md",
   quickstart3 = "doc/quickstart3.md",
   tutorial = "doc/quickstart.md", -- aliases for quickstart
   help = "doc/quickstart.md",

   luabook1 = "doc/lua-1-intro.md",
   luabook2 = "doc/lua-2-expressions.md",
   luabook3 = "doc/lua-3-statements.md",
   -- TODO: these ones still need a lot of formatting tweaks
   luabook4 = "doc/lua-4-functions.md",
   luabook5 = "doc/lua-5-tables.md",
   luabook6 = "doc/lua-6-libs.md",

   engine = "doc/engine.md",
   cargo_bay = "doc/cargo_bay.md",
   fuel_tank = "doc/fuel_tank.md",
   fuel_charger = "doc/fuel_charger.md",
   solar_panel = "doc/solar_panel.md",
   comm_boost = "doc/comm_boost.md",
   life_support = "doc/life_support.md",
   map = "doc/map.md",
   jeejah = "doc/jeejah.md",
   bencode_decoder = "doc/bencode_decoder.md",
   underclocker = "doc/underclocker.md",
}

local show_page = function(ship, p)
   if(lume.find(upgrades, p)) then
      return lume.find(ship.upgrade_names, p)
   elseif(events[p]) then
      return ship.events[events[p]]
   else
      return true
   end
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
         ship.api.print(list(ship))
      elseif(pages[page_name] and show_page(ship, page_name)) then
         -- can't inline this because read returns multiple values
         local p = love.filesystem.read(pages[page_name])
         ship.api.print(p)
      elseif(page_name) then
         ship.api.print("Page not found.\n\n" .. list(ship))
      else
         local p = love.filesystem.read("doc/quickstart.md")
         ship.api.print(p)
      end

      return ship.api.editor.invisible
   end,

   get = function(page_name)
      local p = love.filesystem.read(pages[page_name])
      return p
   end
}

