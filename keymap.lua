local keymap

local ctrlp = function()
   return love.keyboard.isDown("lctrl", "rctrl", "capslock")
end

local altp = function()
   return love.keyboard.isDown("lalt") or love.keyboard.isDown("ralt")
end

local find_binding = function(key, mode)
   local mode = keymap.modes[mode or keymap.current_mode]
   local map = (ctrlp() and mode.ctrl_map) or (altp() and mode.alt_map) or mode.map

   return map[key] or map["__any"]
end

keymap = {
   modes = {},
   current_mode = "flight",

   define_mode = function(name)
      keymap.modes[name] = {map = {}, ctrl_map = {}, alt_map = {}}
   end,

   define = function(mode, mod, key, fn)
      if(type(mode) == "table") then
         for _,m in ipairs(mode) do
            keymap.define(m, mod, key, fn)
         end
      else
         local map = mod and mod .. "_map" or "map"
         keymap.modes[mode][map][key] = fn
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
