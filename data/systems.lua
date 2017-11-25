-- cargo types:
-- ore
-- food
-- medicine
-- equipment

local _ = { r=0, mass=0,
            texture_type="", -- or texture, occasionally
            name="", os="orb",
            industry=0, tech=0,
            remote=0, pop=0,
            agri=0, mineral=0,
            upgrades={},
}

---- inputs:
-- this is not a linear scale; Earth has 10 billion people, while the largest
-- extrasolar worlds (Sungai, Bohk Prime) have about 100 million.
-- pop: 1: 1000, 2: 10,000, 3: 100,000, 4: 1m, 5: 10m, 6: 100m, 7: 1b, 8: 10b
--
-- pop
-- industry
-- remote
-- agri
-- mineral
-- tech
-- upgrades
--   cargo_bay
--   fuel_tank
--   engine
--   solar_panel
--   fuel_charger
--   life_support (available everywhere)
--   map (available everywhere)

-- The positions for these systems are based on a listing of all habitable stars
-- within 20 light years of Sol from Project Rho:
-- http://www.projectrho.com/public_html/starmaps/mapindex.php#winch
-- Most but not all of the stars have been given colloquial names, a couple have
-- been omitted to tidy things up a bit, compare
-- https://p.hagelb.org/starmap.png vs https://p.hagelb.org/bussard_map.png

local sys = {
   -- Tana
   ["Tana"] =
      {gov="Tana", capitol = true,
       x = -4.8, y=-2.4,
       asteroids = 4,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=230000,
           name="Tana", star=true},
          {r=15000, mass=200,
           texture_type="grassy_oceany",
           name="Malioboro", os="orb", world=true,

           industry=7,tech=5,
           remote=2, pop=3,
           agri=5, mineral=2,
           upgrades={"cargo_bay"},
          },
          {r=45000, mass=500,
           texture_type="gas",
           name="Kota Baru", os="orb", world=true,
           industry=8,tech=6,
           remote=2, pop=4,
           agri=4, mineral=2,
           upgrades={"engine", "cargo_bay"},
          },
          {r=33000, mass=100, station=true,
           image_name="station-pointed",
           name="Telemoyo Station", os="orb", world=true,

           industry=8,tech=6,
           remote=2, pop=1,
           agri=0, mineral=3,
           upgrades={"cargo_bay"},
          },
          {name = "Portal: L 668-21",
           image_name="portal-1",
           r=26000, mass=60, portal="L 668-21", os="lisp"},
          {name = "Portal: Wolf 294",
           image_name="portal-1",
           r=38000, mass=60, portal="Wolf 294", os="lisp"},
          {name = "Portal: Luyten's Star",
           image_name="portal-1",
           r=22500, mass=60, portal="Luyten's Star", os="lisp"}},
      },
   ["Wolf 294"] = -- second-largest Tana system
      {gov="Tana", x=-5.3, y=-0.3,
       asteroids = 7,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=160000,
           name="Wolf 294", star=true},
          {r=25000, mass=500,
           image_name="planet-14", -- ice
           name="Solotogo", os="orb", world=true,

           industry=5,tech=4,
           remote=4, pop=3,
           agri=0, mineral=4,
           upgrades={},
          },
          {name = "Portal: Tana",
           image_name="portal-1",
           r=16000, mass=60, portal="Tana", os="lisp"}
       },
      },
   ["Luyten's Star"] = -- gateway to sol
      {gov="Tana", x=-3.3, y=-2,
       asteroids = 3,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=290000,
           name="Luyten's Star", star=true },
          {r=32000, mass=120,
           image_name="station-pointed", station=true,
           name="Kembali Station", os="orb", world=true,

           industry=6,tech=6,
           remote=5, pop=2,
           agri=0, mineral=2,
           upgrades={"engine", "fuel_tank"},
          },
          {r=12000, mass=520,
           image_name="shaber2", world=true,
           name="Bangunan",},
          {name = "Portal: Tana",
           image_name="portal-1",
           r=25000, mass=60, portal="Tana", os="lisp"},
          {name = "Interportal: Sol",
           image_name="portal-2", interportal=true,
           r=20000, mass=60, portal="Sol", os="lisp"},
       },
      },
   ["L 668-21"] = -- remote mining system
      {gov="Tana",
       x=-3.6, y=-4.1,
       asteroids = 12,
       bodies = {
          {r=0, x=0, y=0, dx=0, dy=0, mass=200000,
           name="L 668-21", star=true },
          {r=20000, mass=700, station=true,
           image_name="station-pointed",
           name="Merdeka Station", os="orb", world=true,

           industry=4,tech=3,
           remote=7, pop=2,
           agri=0, mineral=6,
           upgrades={"battery"},
          },
          {name = "Portal: Tana",
           image_name="portal-1",
           r=29000, mass=160, portal="Tana", os="lisp"}
       },
      },


   -- Sol
   ["Sol"] = {gov="Sol", capitol = true,
              x=0, y=0, asteroids = 1,
              bodies = {
                 {r=0, x=0, y=0, dx=0, dy=0, mass=320000,
                  angle=math.rad(7.25), rot=14.18,
                  name="Sol", star=true, texture_name="sol.jpg"},
                 {r=10000, mass=220, angle=math.rad(0.03), rot=6.14,
                  texture_name="mercury.jpg", world=true,
                  name="Mercury"},
                 {r=14000, mass=300, angle=math.rad(2.64), rot=-1.48,
                  texture_name="venus.jpg", os="lisp", world=true,
                  name="Venus", upgrades={}},
                 {r=20000, mass=400, rot=361,
                  texture_name="earth.png", angle=-0.408,
                  name="Earth", os="orb", world=true,

                  industry=9,tech=7,
                  remote=1, pop=8,
                  agri=9, mineral=2,
                  upgrades={"fuel_tank","engine"},
                 },
                 {r=25000, mass=120,
                  image_name="newton", station=true,
                  name="Newton Station", os="orb", world=true,

                  industry=7,tech=9,
                  remote=1, pop=4,
                  agri=0, mineral=4,
                  upgrades={"fuel_charger", "fuel_tank"},
                 },
                 {r=29000, mass=310, angle=math.rad(25.19), rot=13.18,
                  texture_name="mars.jpg",
                  name="Mars", os="orb", world=true,

                  industry=8,tech=6,
                  remote=1, pop=4,
                  agri=0, mineral=5,
                  upgrades={"engine"},
                 },
                 {r=44000, mass=900, angle=math.rad(3.13), rot=870.54,
                  texture_name="jupiter.jpg", world=true,
                  name="Jupiter"},
                 {r=51000, mass=800, angle=math.rad(26.73), rot=810.79,
                  -- TODO: dang, rings!
                  texture_name="saturn.jpg", world=true,
                  name="Saturn"},
                 {r=58000, mass=100, station=true,
                  image_name="nee-soon",
                  name="Nee Soon Station", os="orb", world=true,

                  industry=5,tech=4,
                  remote=2, pop=2,
                  agri=0, mineral=6,
                  upgrades={"fuel_charger", "bencode_decoder"},
                 },
                 {r=64000, mass=700, angle=math.rad(82.23), rot=-501.16,
                  texture_name="uranus.jpg", world=true,
                  name="Uranus", upgrades={}},
                 {r=70000, mass=700, angle=math.rad(28.32), rot=536.16,
                  texture_name="neptune.jpg", world=true,
                  name="Neptune", os="orb",
                  industry = 2, tech = 5,
                  remote = 3, pop = 1, agri = 0, mineral = 7,
                  upgrades={},
                 },
                 {name = "Interportal: Luyten's Star",
                  image_name="portal-2",
                  r=33000, mass=60, portal="Luyten's Star",
                  interportal=true, os="lisp"},
                 {name = "Interportal: Ross",
                  image_name="portal-2",
                  r=35000, mass=60, portal="Ross",
                  interportal=true, os="lisp"},
                 {name = "Interportal: Lalande",
                  image_name="portal-2",
                  r=37000, mass=60, portal="Lalande",
                  interportal=true, os="lisp"}
              }, -- it's Sol.
   },


   -- Terran
   ["Lalande"] = {gov="Terran", capitol = true,
                  x=1.6, y=-0.4,
                  bodies = {
                     {r=0, x=0, y=0, dx=0, dy=0, mass=200000,
                      name="Lalande 25372", star=true},
                     {r=14000, mass=200,
                      image_name="shaber8",
                      name="Bendera", os="orb", world=true,

                      industry=6,tech=7,
                      remote=2, pop=3,
                      agri=8, mineral=3,
                      upgrades={"fuel_tank", "fuel_charger"},
                     },
                     {r=37000, mass=200,
                      image_name="planet-1",
                      name="Sungai", os="orb", world=true,

                      industry=7,tech=8,
                      remote=2, pop=6,
                      agri=4, mineral=3,
                      upgrades={"cargo_bay", "fuel_charger"},
                     },
                     {r=42000, mass=440, world=true,
                      image_name="planet-15", name="Ipah"},
                     {name = "Portal: Ross",
                      image_name="portal-1",
                      r=20000, mass=60, portal="Ross", os="lisp"},
                     {name = "Interportal: Sol",
                      image_name="portal-2", interportal=true,
                      r=25000, mass=60, portal="Sol", os="lisp"},
                     {name = "Interportal: Bohk",
                      image_name="portal-2", interportal=true,
                      r=29000, mass=60, portal="Bohk", os="lisp"}},
   },
   ["Ross"] = {gov="Terran",
               x=0, y=-1.7, asteroids=2,
               bodies = {
                  {r=0, x=0, y=0, dx=0, dy=0, mass=410000,
                   name="Ross 128", star=true},
                  {r=35000, mass=200, gov="darush",
                   image_name="shaber3", -- dark red volcanic
                   name="Istana", os="orb", world=true,

                   industry=4,tech=6,
                   remote=4, pop=3,
                   agri=1, mineral=6,
                   upgrades={"fuel_tank"},
                  },
                  {r=17000, mass=110,
                   image_name="tribase", station=true,
                   name="Kuching Station", os="orb", world=true,

                   industry=3,tech=5,
                   remote=4, pop=4,
                   agri=0, mineral=3,
                   upgrades={"fuel_tank", "fuel_charger", "bencode_decoder"},
                  },
                  {name = "Interportal: Sol",
                   image_name="portal-2", interportal=true,
                   r=25000, mass=60, portal="Sol", os="lisp"},
                  {name = "Portal: Lalande",
                   image_name="portal-1",
                   r=21000, mass=60, portal="Lalande", os="lisp"}
               },
   },


   -- Bohk
   ["Bohk"] = {gov="Bohk", capitol = true,
               x=3.2, y=-0.6,
               bodies = {
                  {r=0, x=0, y=0, dx=0, dy=0, mass=320000,
                   name="Bohk 832", star=true},
                  {r=15000, mass=500,
                   image_name = "shaber1",
                   name="Bohk Prime", os="orb", world=true,

                   industry=6,tech=6,
                   remote=4, pop=6,
                   agri=4, mineral=2,
                   upgrades={"cargo_bay", "solar_panel"},
                  },
                  {r=20000, mass=100, station=true,
                   image_name = "station-round",
                   name="Warnabu Station", os="orb", world=true,

                   industry=7,tech=8,
                   remote=4, pop=2,
                   agri=0, mineral=3,
                   upgrades={"cargo_bay", "engine", "bencode_decoder"},
                  },
                  {name = "Interportal: Lalande",
                   image_name="portal-2", interportal=true, os="lisp",
                   r=30000, mass=60, portal="Lalande",},
                  {name = "Portal: New Phobos",
                   image_name="portal-1",
                   r=32000, mass=60, portal="New Phobos", os="lisp",},
                  {name = "Portal: Mecalle",
                   image_name="portal-1",
                   r=34000, mass=60, portal="Mecalle", os="lisp"},
               },
   },
   ["New Phobos"] = {gov="Bohk", x=3.9, y=0.2, asteroids=5,
                     bodies = {
                        {r=0, x=0, y=0, dx=0, dy=0, mass=390000,
                         name="New Phobos 4523", star=true},
                        {r=18000, mass=630,
                         image_name = "shaber4",
                         name="Sutep", os="orb", world=true,

                         industry=2,tech=4,
                         remote=4, pop=3,
                         agri=6, mineral=3,
                         upgrades={},
                        },
                        {r=30000, mass=440, world=true,
                         image_name = "shaber7",
                         name="Changlun",

                         industry=2,tech=6,
                         remote=4, pop=4,
                         agri=5, mineral=4,
                         upgrades={"cargo_bay", "fuel_tank"},
                        },
                        {name = "Portal: Bohk",
                         image_name="portal-1",
                         r=24000, mass=60, portal="Bohk", os="lisp"},
                     }, -- tourism center
   },
   ["Mecalle"] = {gov="Bohk",
                  x=2.8, y=0.2,
                  bodies = {
                     {r=0, x=0, y=0, dx=0, dy=0, mass=220000,
                      name="Mecalle 8760", star=true},
                     {r=27000, mass=342,
                      image_name = "planet-11",
                      name="Rimoei", os="orb", world=true,

                      industry=2,tech=3,
                      remote=7, pop=3,
                      agri=7, mineral=5,
                      upgrades={},
                     },
                     {name = "Portal: Bohk",
                      image_name="portal-1",
                      r=18000, mass=60, portal="Bohk", os="lisp"},
                     {name = "Interportal: Katilay",
                      image_name="portal-2", interportal=true,
                      r=22000, mass=60, portal="Katilay", os="lisp"}
                  }, -- smaller border town
   },


   -- Katilay
   ["Katilay"] = {gov="Katilay", capitol = true,
                  x=3.2, y=3.3, asteroids = 3,
                  bodies = {
                     {r=0, x=0, y=0, dx=0, dy=0, mass=210000,
                      name="Katilay 103039", star=true},
                     {r=25000, mass=350,
                      image_name="shaber10",
                      name="Katilay Prime", os="orb", world=true,
                      industry=2,tech=1,
                      remote=8, pop=2,
                      agri=2, mineral=3,
                      upgrades={},
                     },
                     {r=12000, mass=100, station=true,
                      image_name="station-round",
                      name="Mogok Station", os="orb", world=true,
                      industry=3,tech=3,
                      remote=8, pop=3,
                      agri=1, mineral=3,
                      upgrades={},
                     },
                     {name = "Interportal: Mecalle",
                      image_name="portal-2", interportal=true,
                      r=36000, mass=60, portal="Mecalle", os="lisp"}
                  }, -- quiet, isolated. internal conflict.
   },
}

return sys
