local orb = require("os/orb")
local img = love.graphics.newImage

-- cargo types:
-- ore
-- food
-- medicine
-- equipment
-- water

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
          {r=39000, mass=700,
           image=img('assets/planet-3.png'),
           name="Tana Prime", os=orb,
           upgrade_prices = { engine = 512, cargo_bay = 1024, },
           fuel_price = 1,
           account_price = 128,
           prices={food = {stock=90, buy=100, sell=120},
                   ore = {stock=65, buy=150, sell=130},
                   medicine = {stock=40, buy=220, sell=200},
                   equipment = {stock=20, buy=200, sell=180},
                  },
          },
          {r=21000, mass=500,
           image=img('assets/planet-1.png'),
           name="Lioboro", os=orb,
           fuel_price = 1,
           upgrade_prices = { engine = 500, },
           account_price = 128,
           prices={food = {stock=120, buy=90, sell=110},
                   ore = {stock=65, buy=150, sell=130},
                   medicine = {stock=30, buy=230, sell=210},
                   equipment = {stock=10, buy=210, sell=190},
                   water = {stock=50, buy=30, sell=45},
                  }
          },
          {r=35000, mass=700,
           image=img('assets/station-pointed.png'),
           name="Kenapa Station", os=orb,
           fuel_price = 1,
           upgrade_prices = { engine = 512, },
           account_price = 172,
           prices={food = {stock=20, buy=100, sell=120},
                   ore = {stock=40, buy=150, sell=130},
                   medicine = {stock=5, buy=230, sell=200},
                   equipment = {stock=10, buy=210, sell=180},
                  },
          },
          {name = "Portal: L 668-21",
           image = img("assets/portal-1.png"),
           r=26000, mass=75, portal="L 668-21"},
          {name = "Portal: Wolf 294",
           image = img("assets/portal-1.png"),
           r=37000, mass=75, portal="Wolf 294"},
          {name = "Portal: Luyten's Star",
           image = img("assets/portal-1.png"),
           r=20000, mass=75, portal="Luyten's Star"}},
      },
   ["Wolf 294"] = -- second-largest Tana system
      {civ="Tana", x=-5.3, y=-0.3,
       asteroids = 4,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=160000,
           image=img('assets/sun.png'),
           name="Wolf 294", star=true},
          {r=37000, mass=350,
           image=img('assets/planet-9.png'),
           name="Belanda", os=orb,
           fuel_price = 1,
           upgrade_prices = { laser = 312, engine = 640 },
           account_price = 148,
           prices={food = {stock=60, buy=90, sell=110},
                   ore = {stock=40, buy=160, sell=140},
                   medicine = {stock=20, buy=240, sell=200},
                  }
          },
          {r=31000, mass=500,
           image=img('assets/planet-14.png'),
           name="Solotogo", os=orb,
           fuel_price = 1,
           account_price = 148,
           upgrade_prices = { laser = 312, engine = 640 },
           prices={food = {stock=120, buy=90, sell=110},
                   medicine = {stock=30, buy=240, sell=210},
                   equipment = {stock=10, buy=210, sell=190},
                   water = {stock=60, buy=20, sell=30},
                  }
          },
          {name = "Portal: Wolf 1453",
           image = img("assets/portal-1.png"),
           r=26000, mass=75, portal="Wolf 1453", }
      },
   },
   ["Luyten's Star"] = -- gateway to sol
      {civ="Tana", x=-3.3, y=-2, bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=290000,
           image=img('assets/sun.png'),
           name="Luyten's Star", star=true },
          {r=32000, mass=120,
           image=img('assets/station-pointed.png'),
           name="Apkabar station", os=orb,
           fuel_price = 1,
           upgrade_prices = { engine = 600, fuel_tank = 768, },
           account_price = 148,
           prices={
              food={stock=10, buy=90, sell=110,},
              ore={stock=8, buy=120, sell=100},
              water={stock=2, buy=45, sell=65},
           },
          },
          {name = "Portal: Wolf 1453",
           image = img("assets/portal-1.png"),
           r=25000, mass=75, portal="Wolf 1453", },
          {name = "Portal: Sol",
           image = img("assets/portal-2.png"),
           r=20000, mass=75, portal=nil, },
      },
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
           fuel_price = 1,
           account_price = 128,
           upgrade_prices = {
              laser = 256,
           },
           prices={
              food={stock=10, buy=100, sell=110,},
              ore={stock=8, buy=90, sell=50},
              water={stock=2, buy=45, sell=65},
           },
          },
          {name = "Portal: Wolf 1453",
           image = img("assets/portal-1.png"),
           r=29000, mass=75, portal="Wolf 1453", }
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
