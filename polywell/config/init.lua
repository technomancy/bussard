local editor = require("polywell")

require("polywell.config.edit")
require("polywell.config.lua_mode")
require("polywell.config.console")

-- uncomment this out to enable Emacs key bindings, which conflict with
-- some of the more conventional bindings.
-- require("polywell.config.emacs_keys")

-- editor.set_color("text", {200, 255, 200})

editor.open(nil, "*console*")
