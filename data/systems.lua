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
          {x=0, y=0, dx=0, dy=0, mass=230000,
           image=img('assets/sun.png'),
           name="Wolf 1453", star=true },
          {x=30000, y=-27000, dx=-5, dy=5, mass=700,
           image=img('assets/planet-1.png'),
           name="Koria", os=orb,
          },
          {x=10000, y=-29000, dx=-5, dy=5, mass=500,
           image=img('assets/planet-3.png'),
           name="Lioboro", os=orb,
          },
          {name = "Portal: L 668-21",
           image = img("assets/portal-1.png"),
           x=-28000, y=20000, dx=5, dy=-3,
           mass=75, portal="L 668-21"}},
      },
   ["Wolf 294"] = {civ="Tana", x=-5.3, y=-0.3,
                   bodies = {}, -- second-largest Tana system
   },
   ["Luyten's Star"] = {civ="Tana", x=-3.3, y=-2,
                        bodies = {}, -- gateway to sol
   },
   ["L 668-21"] =
      {civ="Tana",
       x=-3.6, y=-4.1,
       asteroids = 8,
       bodies = { {x=0, y=0, dx=0, dy=0, mass=200000,
                   image=img('assets/sun.png'),
                   name="L 668-21", star=true },
          {x=30000, y=27000, dx=-5, dy=5, mass=100,
           image=img('assets/station-pointed.png'),
           name="Mirduka station", os=orb,
           prices={["Food"]={stock=10, buy_price=100, sell_price=110}},
          },
          {name = "Portal: Wolf 1453",
           image = img("assets/portal-1.png"),
           x=-28000, y=20000, dx=5, dy=-15,
           mass=75, portal="Wolf 1453", }
       }, -- remote mining system
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
