-- which rpc functions from the top-level rpcs.lua table are used by orb?
return {
   "get_prompt", "set_prompt",
   "cargo_transfer", "loan", "fine", "port",
   "buy_upgrade", "sell_upgrade", "list_upgrades", "upgrade_help", "buy_user",
   "refuel", "fuel_price", "cargo_prices", "cargo_amounts", "cargo_hold",
   "record_event",
}
