local orb = require("os/orb")
local img = love.graphics.newImage

-- TODO: specify an orbit distance for bodies and calculate x, y, dx, and dy

return {
   -- Tana
   ["Wolf 1453"] =
      {civ="Tana", capitol = true,
       x = -5.8, y=-2.4,
       asteroids = 2,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=230000,
           image=img('assets/sun.png'),
           name="Wolf 1453", star=true },
          {r=35000, mass=700,
           image=img('assets/planet-1.png'),
           name="Koria", os=orb,
          },
          {r=21000, mass=500,
           image=img('assets/planet-3.png'),
           name="Lioboro", os=orb,
          },
          {name = "Portal: L 668-21",
           image = img("assets/portal-1.png"),
           r=26000, mass=75, portal="L 668-21"}},
      },
   ["Wolf 294"] = -- second-largest Tana system
      {civ="Tana", x=-5.3, y=-0.3, bodies = {},
   },
   ["Luyten's Star"] = -- gateway to sol
      {civ="Tana", x=-3.3, y=-2, bodies = {},
   },
   ["L 668-21"] = -- remote mining system
      {civ="Tana",
       x=-3.6, y=-4.1,
       asteroids = 8,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=200000,
           image=img('assets/sun.png'),
           name="L 668-21", star=true },
          {r=23000, mass=100,
           image=img('assets/station-pointed.png'),
           name="Mirduka station", os=orb,
           prices={
              ["Food"]={stock=10, buy_price=100, sell_price=110,},
              ["Ore"]={stock=8, buy_price = 90, sell_price = 50,}
           },
          },
          {name = "Portal: Wolf 1453",
           image = img("assets/portal-1.png"),
           r=26000, mass=75, portal="Wolf 1453", }
       },
      },


   -- Sol
   ["Sol"] = {civ="Sol", capitol = true,
              x=0, y=0,
              bodies = {}, -- it's Sol.
   },


   -- Moyla
   ["Lalande 25372"] = {civ="Moyla", capitol = true,
                        x=1.6, y=-0.4,
                        bodies = {},
   },
   ["Ross 128"] = {civ="Moyla",
                   x=0, y=-1.7,
                   bodies = {},
   },


   -- Bohk
   ["L 354-89"] = {civ="Bohk", capitol = true,
                   x=4.3, y=-0.6,
                   bodies = {},
   },
   ["BD-12°4623"] = {civ="Bohk",
                     x=3.9, y=0.2,
                     bodies = {}, -- tourism center
   },
   ["Lacaille 8760"] = {civ="Bohk",
                        x=2.8, y=0.2,
                        bodies = {}, -- smaller border town
   },


   -- Wadee
   ["Hip 103039"] = {civ="Wadee", capitol = true,
                     x=3.9, y=2.3,
                     bodies = {}, -- rather backwater
   },


   -- Yueh
   ["Delta Pavonis"] = {civ="Yueh", capitol = true,
                        x=4.5, y=-2.6,
                        bodies = {}, -- bustling trade hub
   },
   ["Sigma Draconis"] = {civ="Yueh",
                         x=4.3, y=-1.3,
                         bodies = {},
   },
   ["Wolf 1481"] = {civ="Yueh",
                    x=4.1, y=1.7,
                    bodies = {},
   },
   ["LHS 451"] = {civ="Yueh",
                  x=4.9,y=-1.1,
                  bodies = {}, -- mostly uninhabited
   },
   ["L 205-128"] = {civ="Yueh",
                    x= 5.2, y=-2.3,
                    bodies = {}, -- mostly uninhabited
   },
   ["CD-40 9712"] = {civ="Yueh",
                     x=5.3, y=-2.8,
                     bodies = {}, -- mostly uninhabited
   },
}
