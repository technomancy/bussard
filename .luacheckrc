-- -*- lua -*-
color = false
exclude_files = {"metatable_monkey.lua"}
ignore = {"21/_.*"} -- don't warn when _ is an unused argument

-- Different contexts have different rules about what globals are OK. Engine
-- code is the strictest, while sandboxed code can be looser.

stds.engine = {globals = {"love"}}

stds.inship = {globals = {"ship", "define_mode", "bind", "flight_draw",
                          "editor", "ssh_prompt", "portal", "lume", "utf8",
                          "universe", "graphics", "image", "hsv", "toggle_fps",
                          "pack", "pps", "realprint", "inbox", "utils", "mail",
                          "replyable", "reply", "ssh", "ssh_connect", "tetris"}}

stds.inos = {globals = {"orb", "cargo_prices", "cargo_amounts", "cargo_hold",
                        "cargo_transfer", "io", "lume", "loan", "pps",
                        "set_prompt", "get_prompt", "refuel", "fuel_price",
                        "list_upgrades", "buy_upgrade", "sell_upgrade",
                        "upgrade_help", "port", "door", "record_event"}}

stds.data = {globals = {"love"}}
