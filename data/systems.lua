local orb = require("os.orb")

-- cargo types:
-- ore
-- food
-- medicine
-- equipment

---- inputs: (1 to 10 scale)
-- industry
-- remote
-- agri
-- mineral
-- tech
-- pop
-- upgrades

return {
   -- Tana
   ["Tana"] =
      {civ="Tana", capitol = true,
       x = -5.8, y=-2.4,
       asteroids = 2,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=230000,
           image_name="sun",
           name="Tana", star=true },
          {r=46000, mass=400,
           image_name = "planet-3",
           name="Tana Prime", os=orb,

           industry=8,tech=6,
           remote=2, pop=8,
           agri=4, mineral=2,
           upgrades={"engine", "cargo_bay"},
          },
          {r=16000, mass=500,
           image_name = "planet-1",
           name="Lioboro", os=orb,

           industry=7,tech=5,
           remote=2, pop=5,
           agri=5, mineral=2,
           upgrades={"cargo_bay"},
          },
          {r=35000, mass=700,
           image_name="station-pointed",
           name="Kenapa Station", os=orb,

           industry=8,tech=6,
           remote=2, pop=3,
           agri=0, mineral=3,
           upgrades={"cargo_bay"},
          },
          {name = "Portal: L 668-21",
           image_name="portal-1",
           r=21000, mass=75, portal="L 668-21"},
          {name = "Portal: Wolf 294",
           image_name="portal-1",
           r=37000, mass=75, portal="Wolf 294"},
          {name = "Portal: Luyten's Star",
           image_name="portal-1",
           r=19000, mass=75, portal="Luyten's Star"}},
      },
   ["Wolf 294"] = -- second-largest Tana system
      {civ="Tana", x=-5.3, y=-0.3,
       asteroids = 4,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=160000,
           image_name="sun",
           name="Wolf 294", star=true},
          {r=37000, mass=350,
           image_name="planet-9", -- green
           name="Belanda", os=orb,

           industry=5,tech=4,
           remote=4, pop=1,
           agri=0, mineral=6,
           upgrades={},
          },
          {r=31000, mass=500,
           image_name="planet-14", -- ice
           name="Solotogo", os=orb,

           industry=5,tech=4,
           remote=4, pop=2,
           agri=0, mineral=4,
           upgrades={"laser"},
          },
          {name = "Portal: Tana",
           image_name="portal-1",
           r=26000, mass=75, portal="Tana", }
      },
   },
   ["Luyten's Star"] = -- gateway to sol
      {civ="Tana", x=-3.3, y=-2, bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=290000,
           image_name="sun",
           name="Luyten's Star", star=true },
          {r=32000, mass=120,
           image_name="station-pointed",
           name="Apkabar station", os=orb,

           industry=6,tech=6,
           remote=6, pop=3,
           agri=0, mineral=2,
           upgrades={"fuel_tank"},
          },
          {name = "Portal: Tana",
           image_name="portal-1",
           r=25000, mass=75, portal="Tana", },
          {name = "Portal: Sol",
           image_name="portal-2", interportal=true,
           r=20000, mass=75, portal="Sol", },
      },
   },
   ["L 668-21"] = -- remote mining system
      {civ="Tana",
       x=-3.6, y=-4.1,
       asteroids = 8,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=200000,
           image_name="sun",
           name="L 668-21", star=true },
          {r=23000, mass=100,
           image_name="station-pointed",
           name="Mirduka station", os=orb,

           industry=4,tech=3,
           remote=9, pop=3,
           agri=0, mineral=6,
           upgrades={"laser"},
          },
          {name = "Portal: Tana",
           image_name="portal-1",
           r=29000, mass=75, portal="Wolf 1453", }
       },
      },


   -- Sol
   ["Sol"] = {civ="Sol", capitol = true,
              x=0, y=0,
              bodies = {
                 {r=0, x=0, y=0, dx=0, dy=0, mass=320000,
                  image_name="sol",
                  name="Sol", star=true},
                 {r=11000, mass=220,
                  image_name="mercury",
                  name="Mercury"},
                 {r=16000, mass=400,
                  image_name="venus",
                  name="Venus"},
                 {r=25000, mass=500,
                  image_name="earth",
                  name="Earth", os=orb,

                  industry=9,tech=7,
                  remote=1, pop=9,
                  agri=6, mineral=2,
                  upgrades={},
                 },
                 {r=28000, mass=120,
                  image_name="newton",
                  name="Newton Station", os=orb,

                  industry=7,tech=9,
                  remote=1, pop=4,
                  agri=0, mineral=4,
                  upgrades={},
                 },
                 {r=31000, mass=310,
                  image_name="mars",
                  name="Mars", os=orb,

                  industry=8,tech=6,
                  remote=1, pop=6,
                  agri=0, mineral=5,
                  upgrades={},
                 },
                 {r=37000, mass=900,
                  image_name="jupiter",
                  name="Jupiter"},
                 {r=46000, mass=800,
                  image_name="saturn",
                  name="Saturn"},
                 {r=52000, mass=100,
                  image_name="nee-soon",
                  name="Nee Soon Station", os=orb,

                  industry=5,tech=4,
                  remote=2, pop=2,
                  agri=0, mineral=6,
                  upgrades={"laser"},
                 },
                 {r=59000, mass=700,
                  image_name="uranus",
                  name="Uranus"},
                 {r=68000, mass=700,
                  image_name="neptune",
                  name="Neptune"},
                 {name = "Portal: Luyten's Star",
                  image_name="portal-2",
                  r=20000, mass=75, portal="Luyten's Star",
                  interportal=true,}
              }, -- it's Sol.
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
   ["Bohk"] = {civ="Bohk", capitol = true,
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
   ["Wadee"] = {civ="Wadee", capitol = true,
                x=3.9, y=2.3,
                bodies = {}, -- rather backwater
   },


   -- Yueh
   ["Yueh"] = {civ="Yueh", capitol = true,
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
