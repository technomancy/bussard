local keymap

local find_binding = function(key, mode_name)
   local mode = keymap.modes[mode_name or keymap.current_mode]
   local ctrl = love.keyboard.isDown("lctrl", "rctrl", "capslock")
   local alt = love.keyboard.isDown("lalt", "ralt")
   local map = (ctrl and alt and mode["ctrl-alt"]) or
      (ctrl and mode.ctrl) or (alt and mode.alt) or mode.map

   return map[key] or map["__any"]
end

keymap = {
   modes = {},
   current_mode = nil,

   define_mode = function(name)
      keymap.modes[name] = {map = {}, ctrl = {}, alt = {},
                            ["ctrl-alt"] = {}}
      -- first mode to be defined becomes active
      keymap.current_mode = keymap.current_mode or name
   end,

   define = function(mode, keycode, fn)
      if(type(mode) == "table") then
         for _,m in ipairs(mode) do
            keymap.define(m, keycode, fn)
         end
      else
         -- lua regexes don't support |
         local map, key = keycode:match("(ctrl\\-alt)-(%S+)")
         if not map then map, key = keycode:match("(ctrl)-(%S+)") end
         if not map then map, key = keycode:match("(alt)-(%S+)") end
         if map == "alt-ctrl" then map = "ctrl-alt" end
         keymap.modes[mode][map or "map"][key or keycode] = fn
      end
   end,

   handle = function(key, is_repeat)
      local fn = find_binding(key)
      if(fn) then fn(is_repeat) end
   end,

   change_mode = function(mode_name)
      keymap.current_mode = mode_name
   end,

   find_binding = find_binding,

   textinput = function(text)
      if(find_binding(text)) then return end
      if(keymap.modes[keymap.current_mode].textinput) then
         keymap.modes[keymap.current_mode].textinput(text)
      end
   end,
}

return keymap
