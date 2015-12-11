local orb = require("os.orb")
local lisp = require("os.lisp")

-- cargo types:
-- ore
-- food
-- medicine
-- equipment

local _ = { r=0, mass=0,
            image_name="",
            name="", os=orb,
            industry=0, tech=0,
            remote=0, pop=0,
            agri=0, mineral=0,
            upgrades={},
}

---- inputs:
-- this is not a linear scale; Earth has 10 billion people, while the largest
-- extrasolar worlds (Kala Lamar, Yueh Prime) have about 100 million.
-- pop: 1: 1000, 2: 10,000, 3: 100,000, 4: 1m, 5: 10m, 6: 100m, 7: 1b, 8: 10b
-- total: 10,216,655,000, total off-earth: 216,655,000
--
-- pop
-- industry
-- remote
-- agri
-- mineral
-- tech
-- upgrades
--   laser
--   cargo_bay
--   fuel_tank
--   engine
--   solar_panel
--   fuel_charger
--   comm_boost (must be unlocked)
--   map

local sys = {
   -- Tana
   ["Tana"] =
      {gov="Tana", capitol = true,
       x = -5.8, y=-2.4,
       asteroids = 2,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=230000,
           image_name="sun",
           name="Tana", fixed=true },
          {r=45000, mass=400,
           image_name = "planet-3",
           name="Tana Prime", os=orb,

           industry=8,tech=6,
           remote=2, pop=4,
           agri=4, mineral=2,
           upgrades={"engine", "cargo_bay", "map"},
          },
          {r=15000, mass=500,
           image_name = "planet-9",
           name="Lioboro", os=orb,

           industry=7,tech=5,
           remote=2, pop=3,
           agri=5, mineral=2,
           upgrades={"cargo_bay"},
          },
          {r=33000, mass=100, station=true,
           image_name="station-pointed",
           name="Kenapa Station", os=orb,

           industry=8,tech=6,
           remote=2, pop=1,
           agri=0, mineral=3,
           upgrades={"cargo_bay"},
          },
          {name = "Portal: L 668-21",
           image_name="portal-1",
           r=26000, mass=60, portal="L 668-21", os=lisp},
          {name = "Portal: Wolf 294",
           image_name="portal-1",
           r=38000, mass=60, portal="Wolf 294", os=lisp},
          {name = "Portal: Luyten's Star",
           image_name="portal-1",
           r=22500, mass=60, portal="Luyten's Star", os=lisp}},
      },
   ["Wolf 294"] = -- second-largest Tana system
      {gov="Tana", x=-5.3, y=-0.3,
       asteroids = 7,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=160000,
           image_name="sun",
           name="Wolf 294", fixed=true},
          {r=25000, mass=500,
           image_name="planet-14", -- ice
           name="Solotogo", os=orb,

           industry=5,tech=4,
           remote=4, pop=2,
           agri=0, mineral=4,
           upgrades={"laser"},
          },
          {name = "Portal: Tana",
           image_name="portal-1",
           r=16000, mass=60, portal="Tana", os=lisp}
       },
      },
   ["Luyten's Star"] = -- gateway to sol
      {gov="Tana", x=-3.3, y=-2,
       asteroids = 3,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=290000,
           image_name="sun",
           name="Luyten's Star", fixed=true },
          {r=32000, mass=120,
           image_name="station-pointed", station=true,
           name="Apkabar Station", os=orb,

           industry=6,tech=6,
           remote=5, pop=2,
           agri=0, mineral=2,
           upgrades={"engine", "fuel_tank", "map"},
          },
          {r=12000, mass=520,
           image_name="shaber2",
           name="Malong",},
          {name = "Portal: Tana",
           image_name="portal-1",
           r=25000, mass=60, portal="Tana", os=lisp},
          {name = "Portal: Sol",
           image_name="portal-2", interportal=true,
           r=20000, mass=60, portal="Sol", os=lisp},
       },
      },
   ["L 668-21"] = -- remote mining system
      {gov="Tana",
       x=-3.6, y=-4.1,
       asteroids = 12,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=200000,
           image_name="sun",
           name="L 668-21", fixed=true },
          {r=20000, mass=100, station=true,
           image_name="station-pointed",
           name="Mirduka Station", os=orb,

           industry=4,tech=3,
           remote=7, pop=2,
           agri=0, mineral=6,
           upgrades={"laser"},
          },
          {name = "Portal: Tana",
           image_name="portal-1",
           r=29000, mass=60, portal="Tana", os=lisp}
       },
      },


   -- Sol
   ["Sol"] = {gov="Sol", capitol = true,
              x=0, y=0, asteroids = 1,
              bodies = {
                 {r=0, x=0, y=0, dx=0, dy=0, mass=320000,
                  image_name="sol",
                  name="Sol", fixed=true},
                 {r=10000, mass=220,
                  image_name="mercury",
                  name="Mercury"},
                 {r=14000, mass=300,
                  image_name="venus", os=lisp,
                  name="Venus"},
                 {r=20000, mass=400,
                  image_name="earth",
                  name="Earth", os=orb,

                  industry=9,tech=7,
                  remote=1, pop=8,
                  agri=9, mineral=2,
                  upgrades={"fuel_tank","engine", "map"},
                 },
                 {r=25000, mass=120,
                  image_name="newton", station=true,
                  name="Newton Station", os=orb,

                  industry=7,tech=9,
                  remote=1, pop=4,
                  agri=0, mineral=4,
                  upgrades={"fuel_charger", "fuel_tank", "map"},
                 },
                 {r=29000, mass=310,
                  image_name="mars",
                  name="Mars", os=orb,

                  industry=8,tech=6,
                  remote=1, pop=4,
                  agri=0, mineral=5,
                  upgrades={"engine", "laser", "map"},
                 },
                 {r=44000, mass=900,
                  image_name="jupiter",
                  name="Jupiter"},
                 {r=51000, mass=800,
                  image_name="saturn",
                  name="Saturn"},
                 {r=58000, mass=100, station=true,
                  image_name="nee-soon",
                  name="Nee Soon Station", os=orb,

                  industry=5,tech=4,
                  remote=2, pop=2,
                  agri=0, mineral=6,
                  upgrades={"laser", "fuel_charger"},
                 },
                 {r=64000, mass=700,
                  image_name="uranus",
                  name="Uranus"},
                 {r=70000, mass=700,
                  image_name="neptune",
                  name="Neptune"},
                 {name = "Portal: Luyten's Star",
                  image_name="portal-2",
                  r=33000, mass=60, portal="Luyten's Star",
                  interportal=true, os=lisp},
                 {name = "Portal: Ross",
                  image_name="portal-2",
                  r=35000, mass=60, portal="Ross",
                  interportal=true, os=lisp},
                 {name = "Portal: Lalande",
                  image_name="portal-2",
                  r=37000, mass=60, portal="Lalande",
                  interportal=true, os=lisp}
              }, -- it's Sol.
   },


   -- Terran
   ["Lalande"] = {gov="Terran", capitol = true,
                  x=1.6, y=-0.4,
                  bodies = {
                     {r=0, x=0, y=0, dx=0, dy=0, mass=200000,
                      image_name="sun",
                      name="Lalande 25372", fixed=true},
                     {r=14000, mass=200,
                      image_name="shaber8",
                      name="Pinan", os=orb,

                      industry=6,tech=7,
                      remote=2, pop=3,
                      agri=5, mineral=3,
                      upgrades={"fuel_tank", "fuel_charger"},
                     },
                     {r=37000, mass=200,
                      image_name="planet-1",
                      name="Kala Lamar", os=orb,

                      industry=7,tech=8,
                      remote=2, pop=6,
                      agri=4, mineral=3,
                      upgrades={"cargo_bay", "fuel_charger", "map"},
                     },
                     {r=42000, mass=440,
                      image_name="planet-15", name="Ipah"},
                     {name = "Portal: Ross",
                      image_name="portal-1",
                      r=20000, mass=60, portal="Ross", os=lisp},
                     {name = "Portal: Sol",
                      image_name="portal-2", interportal=true,
                      r=25000, mass=60, portal="Sol", os=lisp},
                     {name = "Portal: Bohk",
                      image_name="portal-2", interportal=true,
                      r=29000, mass=60, portal="Bohk", os=lisp},
                     {name = "Portal: Katilay",
                      image_name="portal-2", interportal=true,
                      r=31000, mass=60, portal="Katilay", os=lisp},
                     {name = "Portal: Yueh",
                      image_name="portal-2", interportal=true,
                      r=33000, mass=60, portal="Yueh", os=lisp}},
   },
   ["Ross"] = {gov="Terran",
               x=0, y=-1.7, asteroids=2,
               bodies = {
                  {r=0, x=0, y=0, dx=0, dy=0, mass=410000,
                   image_name="sun",
                   name="Ross 128", fixed=true},
                  {r=35000, mass=200, gov="darush",
                   image_name="shaber3", -- dark red volcanic
                   name="Darush", os=orb,

                   industry=4,tech=6,
                   remote=4, pop=3,
                   agri=1, mineral=6,
                   upgrades={"laser", "fuel_tank"},
                  },
                  {r=17000, mass=110,
                   image_name="tribase", station=true,
                   name="Kuchang Station", os=orb,

                   industry=3,tech=5,
                   remote=4, pop=4,
                   agri=0, mineral=3,
                   upgrades={"fuel_tank", "fuel_charger"},
                  },
                  {name = "Portal: Sol",
                   image_name="portal-2", interportal=true,
                   r=25000, mass=60, portal="Sol",
                   interportal=true, os=lisp},
                  {name = "Portal: Lalande",
                   image_name="portal-1",
                   r=21000, mass=60, portal="Lalande", os=lisp}
               },
   },


   -- Bohk
   ["Bohk"] = {gov="Bohk", capitol = true,
               x=3.2, y=-0.6,
               bodies = {
                  {r=0, x=0, y=0, dx=0, dy=0, mass=320000,
                   image_name="sun",
                   name="Bohk 832", fixed=true},
                  {r=15000, mass=500,
                   image_name = "shaber1",
                   name="Bohk Prime", os=orb,

                   industry=7,tech=6,
                   remote=4, pop=5,
                   agri=4, mineral=2,
                   upgrades={"cargo_bay", "solar_panel", "map"},
                  },
                  {r=20000, mass=100, station=true,
                   image_name = "station-round",
                   name="Warnabu Station", os=orb,

                   industry=7,tech=8,
                   remote=4, pop=1,
                   agri=1, mineral=3,
                   upgrades={"cargo_bay", "engine", "solar_panel"},
                  },
                  {r=25000, mass=430,
                   image_name = "planet-10",
                   name="Banga", os=orb,

                   industry=5,tech=4,
                   remote=5, pop=4,
                   agri=5, mineral=4,
                   upgrades={"cargo_bay"},
                  },
                  {name = "Portal: Lalande",
                   image_name="portal-2", interportal=true, os=lisp,
                   r=30000, mass=60, portal="Lalande",},
                  {name = "Portal: New Phobos",
                   image_name="portal-1",
                   r=32000, mass=60, portal="New Phobos", os=lisp,},
                  {name = "Portal: Mecalle",
                   image_name="portal-1",
                   r=34000, mass=60, portal="Mecalle", os=lisp},
                  {name = "Portal: Yueh",
                   image_name="portal-2", interportal=true,
                   r=36000, mass=60, portal="Yueh", os=lisp}
               },
   },
   ["New Phobos"] = {gov="Bohk", x=3.9, y=0.2, asteroids=5,
                     bodies = {
                        {r=0, x=0, y=0, dx=0, dy=0, mass=390000,
                         image_name="sun",
                         name="New Phobos 4523", fixed=true},
                        {r=18000, mass=630,
                         image_name = "shaber4",
                         name="Sutap", os=orb,

                         industry=2,tech=6,
                         remote=4, pop=4,
                         agri=5, mineral=5,
                         upgrades={"cargo_bay", "fuel_tank", "map"},
                        },
                        {r=30000, mass=440,
                         image_name = "shaber7",
                         name="Changlun",
                        },
                        {name = "Portal: Bohk",
                         image_name="portal-1",
                         r=24000, mass=60, portal="Bohk", os=lisp},
                     }, -- tourism center
   },
   ["Mecalle"] = {gov="Bohk",
                  x=2.8, y=0.2,
                  bodies = {
                     {r=0, x=0, y=0, dx=0, dy=0, mass=220000,
                      image_name="sun",
                      name="Mecalle 8760", fixed=true},
                     {r=27000, mass=342,
                      image_name = "planet-11",
                      name="Tirakir", os=orb,

                      industry=2,tech=3,
                      remote=7, pop=3,
                      agri=7, mineral=5,
                      upgrades={},
                     },
                     {name = "Portal: Bohk",
                      image_name="portal-1",
                      r=18000, mass=60, portal="Bohk", os=lisp},
                     {name = "Portal: Katilay",
                      image_name="portal-2", interportal=true,
                      r=22000, mass=60, portal="Katilay", os=lisp}
                  }, -- smaller border town
   },


   -- Katilay
   ["Katilay"] = {gov="Katilay", capitol = true,
                  x=3.2, y=3.3, asteroids = 3,
                  bodies = {
                     {r=0, x=0, y=0, dx=0, dy=0, mass=210000,
                      image_name="sun",
                      name="Katilay 103039", fixed=true},
                     {r=25000, mass=350,
                      image_name="shaber10",
                      name="Katilay Prime", os=orb,
                      industry=2,tech=1,
                      remote=8, pop=2,
                      agri=2, mineral=3,
                      upgrades={"map"},
                     },
                     {r=12000, mass=100, station=true,
                      image_name="station-round",
                      name="Slork Station", os=orb,
                      industry=3,tech=3,
                      remote=8, pop=3,
                      agri=1, mineral=3,
                      upgrades={"laser"},
                     },
                     {name = "Portal: Lalande",
                      image_name="portal-2", interportal=true,
                      r=32000, mass=60, portal="Lalande", os=lisp},
                     {name = "Portal: Mecalle",
                      image_name="portal-2", interportal=true,
                      r=36000, mass=60, portal="Mecalle", os=lisp}
                  }, -- quiet, isolated. internal conflict.
   },


   -- Yueh
   ["Yueh"] = {gov="Yueh", capital=true, -- GI 674
               x=4.3, y=-1.4, asteroids=2,
               bodies = {
                  {r=0, x=0, y=0, dx=0, dy=0, mass=390000,
                   image_name="sun",
                   name="Yueh 674", fixed=true},
                  {r=14000, mass=350, asteroids=4,
                   image_name="planet-6",
                   name="Yueh Prime", os=lisp,
                   industry=7,tech=8,
                   remote=3, pop=5.8,
                   agri=3, mineral=3,
                   upgrades={"fuel_charger", "laser", "cargo_bay", "map"},
                  },
                  {r=19000, mass=100, station=true,
                   image_name="station-green",
                   name="Da Kau Station", os=orb,
                   industry=6,tech=8,
                   remote=4, pop=1,
                   agri=0, mineral=3,
                   upgrades={"laser", "cargo_bay", "engine", "solar_panel"},
                  },
                  {r=9000, mass=220,
                   image_name="planet-3", name="Ha'nur"},
                  {name = "Portal: Bohk",
                   image_name="portal-2", interportal=true, os=lisp,
                   r=22000, mass=60, portal="Bohk"},
                  {name = "Portal: Lalande",
                   image_name="portal-2", interportal=true, os=lisp,
                   r=28000, mass=60, portal="Lalande"},
                  {name = "Portal: Kowlu",
                   image_name="portal-3", os=lisp, multiportal=true,
                   r=32000, mass=60, portal="Kowlu"},
               }, -- Yueh capital
   },
   ["Kowlu"] = {gov="Yueh", -- GI 693
                x= 5.4, y=-2.3, asteroids=6,
                bodies = {
                   {r=0, x=0, y=0, dx=0, dy=0, mass=440000,
                    image_name="sun",
                    name="Kowlu 693", fixed=true},
                   {r=12000, mass=500,
                    image_name="shaber6",
                    name="Bata Beng", os=orb,
                    industry=5, tech=4,
                    remote=6, pop=4,
                    agri=7, mineral=5,
                    upgrades={"cargo_bay", "engine"},
                   },
                   {r=17000, mass=300,
                    image_name="shaber9",
                    name="Sim Roen", os=orb,
                    industry=2, tech=3,
                    remote=5, pop=1,
                    agri=4, mineral=4,
                    upgrades={"engine"},
                   },
                   {name = "Portal: Delta Pavonis",
                    image_name="portal-1", os=lisp,
                    r=22000, mass=60, portal="Delta Pavonis"},
                   {name = "Portal: Yueh",
                    image_name="portal-1", os=lisp,
                    r=27000, mass=60, portal="Yueh"}
                }, -- bustling trade hub
   },
   ["Delta Pavonis"] = {gov="Yueh",
                        x=4.5, y=-2.6,
                        bodies = {
                           {r=0, x=0, y=0, dx=0, dy=0, mass=190000,
                            image_name="sun",
                            name="Delta Pavonis", fixed=true},
                           {r=18000, mass=300,
                            image_name="v-surface",
                            name="Packsi", os=orb,
                            industry=2, tech=3,
                            remote=6, pop=3,
                            agri=3, mineral=4,
                            upgrades={"engine", "fuel_charger", "map"},
                           },
                           {r=10000, mass=200,
                            image_name="planet-3", name="Sav'nakat"},
                           {name = "Portal: Wolf 1481",
                            image_name="portal-1", os=lisp,
                            r=22000, mass=60, portal="Wolf 1481"},
                           {name = "Portal: Kowlu",
                            image_name="portal-1", os=lisp,
                            r=25000, mass=60, portal="Kowlu"},
                        }
   },
   ["Wolf 1481"] = {gov="Yueh", -- BD
                    x=4.0, y=-2.1, asteroids=3,
                    bodies = {
                       {r=0, x=0, y=0, dx=0, dy=0, mass=150000,
                        image_name="sun",
                        name="Wolf 1481", fixed=true},
                       {r=12000, mass=100, station=true,
                        image_name="station-green",
                        name="Steele Station", os=lisp,},
                       {name = "Lueng Prabo",
                        image_name="shaber5",
                        r=19000, mass=240},
                       {name = "Portal: Delta Pavonis",
                        image_name="portal-1", os=lisp,
                        r=25000, mass=60, portal="Delta Pavonis"}
                    },
   }, -- location of experimental lisp station


   -- secret colonies
   ["LHS 451"] = {gov="Yueh", -- GI 682
                  x=4.9,y=-1.1, unmapped=true,
                  bodies = {
                     {r=0, x=0, y=0, dx=0, dy=0, mass=280000,
                      image_name="sun",
                      name="LHS 451", fixed=true},
                     {name = "Portal: Yueh",
                      image_name="portal-3", os=lisp,
                      r=22000, mass=60, portal="Yueh"},
                  },
   },
   ["CD-40 9712"] = {gov="Yueh", -- GI 588
                     x=5.3, y=-3.1, unmapped=true,
                     bodies = {
                        {r=0, x=0, y=0, dx=0, dy=0, mass=280000,
                         image_name="sun",
                         name="CD-40 9712", fixed=true},
                        {name = "Portal: Yueh",
                         image_name="portal-3", os=lisp,
                         r=22000, mass=60, portal="Yueh"},
                     },
   },
}

return sys
