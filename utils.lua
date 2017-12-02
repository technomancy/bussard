local utf8 = require("polywell.utf8")
local socket = require("socket")
local lume = require("lume")
local serpent = require("serpent")

local pps = function(x)
   return serpent.block(x, {maxlevel=8,maxnum=64,nocode=true})
end

_G["pp"] = function(x) print(pps(x)) end

local original_pairs, original_ipairs = unpack(require("metatable_monkey"))

local seconds_per_year = 365 * 24 * 60 * 60

local shallow_copy = function(orig)
   local orig_type = type(orig)
   local copy
   if orig_type == 'table' then
      copy = {}
      for orig_key, orig_value in pairs(orig) do
         copy[orig_key] = orig_value
      end
   else -- number, string, boolean, etc
      copy = orig
   end
   return copy
end

local includes = function(tab, val)
   for _,x in pairs(tab) do
      if(x == val) then return true end
   end
   return false
end

local function distance(x, y)
   if(type(x) == "number" and type(y) == "number") then
      return math.sqrt(x*x+y*y)
   else -- accept tables
      return distance(x.x - y.x, x.y - y.y)
   end
end

local pad_to = function(s, width, padding)
   padding = padding or " "
   for _=1,width-#s,#padding do
      s = padding .. s
   end
   return s
end

local fnil = function(f, val)
   return function(possibly_nil, ...)
      return f((possibly_nil == nil) and val or possibly_nil, ...)
   end
end

local adder = function(x) return function(y) return x + y end end

local find_common = function(a, b)
   for i=math.min(#a, #b),1,-1 do
      if(utf8.sub(a,1,i) == utf8.sub(b,1,i)) then
         return utf8.sub(a,1,i)
      end
   end
end

local longest_common_prefix = function(strings)
   local common = strings[1]
   for _,s in pairs(strings) do
      common = find_common(common, s)
      if(not common) then return "" end
   end
   return common
end

local function completions_for(input, context, separator, prefixes)
   if(type(context) ~= "table") then return {} end
   prefixes = prefixes or {}
   local input_parts = separator and lume.split(input, separator) or {input}
   if(#input_parts == 1) then
      local matches = {}
      -- if we have an array, use it as-is. if we have a k/v table, use keys.
      if(#context ~= lume.count(context)) then
         context = lume.keys(context)
      end
      for _,v in pairs(context) do
         if(type(v) == "string" and utf8.sub(v, 1, #input) == input) then
            local parts = lume.clone(prefixes)
            table.insert(parts, v)
            table.insert(matches, table.concat(parts, separator))
         end
      end
      return matches
   else
      local first_part = table.remove(input_parts, 1)
      table.insert(prefixes, first_part)
      return completions_for(table.concat(input_parts, separator),
                             context[first_part], separator, prefixes)
   end
end

local epoch_for = function(year)
   local years = year - 1970
   return years * seconds_per_year
end

local format_time = function(s)
   local years = math.floor(s / seconds_per_year) + 1970
   local seconds = s % seconds_per_year
   local formatted, k = tostring(math.ceil(seconds))
   while k ~= 0 do
      formatted, k = string.gsub(formatted, "(-?%d+)(%d%d%d)", '%1,%2')
   end
   return tostring(years) .. ":" .. formatted
end

local parse_time = function(t)
   local y,s = unpack(lume.split(t, ":"))
   local s2 = s:gsub(",", "")
   return tonumber(y) * seconds_per_year + tonumber(s2)
end

local pairs_for = function(raw, wrap)
   return function(_)
      local t = {}
      for k,v in original_pairs(raw) do
         if(type(v) == "table") then
            t[k] = wrap(v)
         else
            t[k] = v
         end
      end
      return next,t,nil
   end
end

local ipairs_for = function(raw, wrap)
   return function(_)
      local t = {}
      for k,v in original_ipairs(raw) do
         if(type(v) == "table") then
            t[k] = wrap(v)
         else
            t[k] = v
         end
      end
      return next,t,nil
   end
end

local function readonly_proxy(source, table_name)
   table_name = table_name or "table"
   local t = {}
   local mt = {
      __index = function(_, key)
         if(type(source[key]) == "table") then
            return readonly_proxy(source[key])
         else
            return source[key]
         end
      end,
      __pairs = pairs_for(source, readonly_proxy),
      __ipairs = ipairs_for(source, readonly_proxy),
      __newindex = function(_, _, _) error(table_name .. " are read-only") end
   }
   setmetatable(t, mt)
   return t
end

local whitelist_pairs = function(source, whitelist)
   return function(_)
      local trimmed = {}
      for k,v in pairs(lume.pick(source, unpack(whitelist))) do
         if(type(v) == "table") then
            trimmed[k] = readonly_proxy(v, k)
         else
            trimmed[k] = v
         end
      end
      return next, trimmed, nil
   end
end

local timers = {}

local timer = function(period, callback)
   timers[callback] = 0
   return function(dt)
      timers[callback] = timers[callback] + dt
      if(timers[callback] >= period) then
         callback(timers[callback])
         timers[callback] = 0
      end
   end
end

local rect_overlap = function(r1, r2)
   local x1, y1, w1, h1 = unpack(r1)
   local x2, y2, w2, h2 = unpack(r2)
   return x1 < x2+w2 and x1+w1 > x2 and y1 < y2+h2 and y1+h1 > y2
end

local with_traceback = function(print2, f, ...)
   local args = {...}
   local wrapped = function()
      if(type(f) == "function") then
         return f(unpack(args))
      else
         return coroutine.resume(f, unpack(args))
      end
   end
   -- TODO: sandboxed traceback which trims out irrelevant layers
   return xpcall(wrapped, function(e)
                    print(debug.traceback())
                    print(e)
                    if(print2) then
                       print2(debug.traceback())
                       print2(e)
                    end
   end)
end

local run_handlers = function(object, handlers_name, broken_name, args, print)
   for n,f in pairs(object[handlers_name]) do
      if not with_traceback(print, f, unpack(args)) then
         object[broken_name] = object[broken_name] or {}
         object[broken_name][n] = f
         object[handlers_name][n] = nil
      end
   end
end

local hsv = function(h, s, v)
   if(s==0) then return v, v, v end
   h, s, v = h/256*6, s/255, v/255
   local c = v*s
   local x = (1-math.abs((h%2)-1))*c
   local m,r,g,b = (v-c)
   if h < 1     then r,g,b = c,x,0
   elseif h < 2 then r,g,b = x,c,0
   elseif h < 3 then r,g,b = 0,c,x
   elseif h < 4 then r,g,b = 0,x,c
   elseif h < 5 then r,g,b = x,0,c
   else              r,g,b = c,0,x
   end
   return (r+m)*255,(g+m)*255,(b+m)*255
end

return {
   shallow_copy = shallow_copy,

   includes = includes,

   starts_with = function(s, start)
      return utf8.sub(s,1,utf8.len(start))==start
   end,

   whitelist_table = function(source, whitelist, table_name)
      table_name = table_name or "table"
      local t = {}
      local mt = {
         __index = function(_, key)
            if(includes(whitelist, key)) then
               if(type(source[key]) == "table") then
                  return readonly_proxy(source[key])
               else
                  return source[key]
               end
            end
         end,
         __pairs = whitelist_pairs(source, whitelist),
         __ipairs = whitelist_pairs(source, whitelist),
         __newindex = function(_, _, _) error(table_name .. " is read-only") end
      }
      setmetatable(t, mt)
      return t
   end,

   readonly_proxy = readonly_proxy,

   distance = distance,

   format_time = format_time,
   parse_time = parse_time,

   sandbox = {
      -- functions
      assert = assert,
      error = error,
      pairs = pairs,
      ipairs = ipairs,
      next = next,
      pcall = pcall,
      xpcall = xpcall,
      select = select,
      tonumber = tonumber,
      tostring = tostring,
      type = type,
      unpack = unpack,
      pack = function(...) return {...} end,
      realprint = print,

      -- tables
      coroutine = lume.clone(coroutine),
      math = lume.merge(math, love.math),
      table = lume.clone(table),
      string = lume.clone(string),
      utf8 = lume.clone(utf8),

      -- custom
      utils = {
         completions_for = completions_for,
         longest_common_prefix = longest_common_prefix,
         pad_to = pad_to,
         distance = distance,
         format_time = format_time,
         parse_time = parse_time,
         rect_overlap = rect_overlap,
         fnil = fnil,
         adder = adder,
      },
      lume = lume.clone(lume),
      pps = pps,
   },

   completions_for = completions_for,
   longest_common_prefix = longest_common_prefix,

   time = function(ship)
      return ship.time_offset + (socket.gettime() - ship.load_time)
         * ship.time_factor
   end,

   timer = timer,

   ptimer = function(period, callback)
      return timer(period, function(x)
                      local ok, err = pcall(function() callback(x) end)
                      if(not ok) then print("Timer error: " .. err) end
      end)
   end,

   rfn = function(fn, ...)
      local partial_args = {...}
      return function(...)
         return fn(unpack(lume.concat({...}, partial_args)))
      end
   end,

   find_by = function(ts, key, value)
      for _,t in ipairs(ts) do
         if(t[key] == value) then return t end
      end
   end,

   find_kv_by = function(ts, field, value)
      for k,v in ipairs(ts) do
         if(v[field] == value) then return k,v end
      end
   end,

   sort_by = function(orig, f)
      local t = lume.clone(orig)
      table.sort(t, function(a, b) return f(a) < f(b) end)
      return t
   end,

   copy_keys = function(to, from, ...)
      for _,k in ipairs({...}) do
         to[k] = from[k]
      end
   end,

   get_in = function(t, ...)
      for _,k in ipairs({...}) do
         t = type(t) == "table" and t[k]
      end
      return t
   end,

   fnil = fnil,
   adder = adder,

   pad_to = pad_to,

   rect_overlap = rect_overlap,

   with_traceback = with_traceback,

   run_handlers = run_handlers,

   hsv = hsv,

   game_start = epoch_for(2431) + 10242852, -- april 17th, 2431
}
