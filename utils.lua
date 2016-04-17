local socket = require("socket")

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
   for i=1,width-#s,#padding do
      s = padding .. s
   end
   return s
end

local format_time = function(s)
   local years = math.floor(s / seconds_per_year) + 1970
   local seconds = math.mod(s, seconds_per_year)
   local formatted, k = tostring(math.ceil(seconds))
   while k ~= 0 do
      formatted, k = string.gsub(formatted, "(-?%d+)(%d%d%d)", '%1,%2')
   end
   return tostring(years) .. ":" .. formatted
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

-- Circular buffer functionality
local buffer = {}

function buffer:new(ob)
   local o = ob or {}
   o.entries = #o
   o.cursor = #o + 1
   o.max = 10
   setmetatable(o, self)
   self.__index = self
   return o
end

function buffer:append(entry, assume_newline)
   if self[self.cursor] then
      self[self.cursor] = entry
      self.cursor = self.cursor + 1
      if self.entries ~= self.max then
         self.entries = self.entries + 1
      end
   elseif(self[#self] and self[#self]:byte(-1) ~= 10 and not assume_newline) then
      self[#self] = self[#self] .. entry
   else
      table.insert(self, entry)
      self.cursor = self.cursor + 1
      if self.entries ~= self.max then
         self.entries = self.entries + 1
      end
   end
   if self.cursor == self.max + 1 then
      self.cursor = 1
   end
end

function buffer:get(idx)
   -- Allow negative indexes
   if idx < 0 then
      idx = (self.entries + idx) + 1
   end

   if self.entries == self.max then
      local c = self.cursor + idx - 1
      if c > self.max then
         c = c - self.max
      end
      return self[c]
   else
      return self[idx]
   end
end

return {
   shallow_copy = shallow_copy,

   includes = includes,

   starts_with = function(s, start)
      return string.sub(s,1,string.len(start))==start
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

      -- tables
      coroutine = lume.clone(coroutine),
      math = lume.clone(math),
      table = lume.clone(table),
      string = lume.clone(string),

      -- custom
      utils = {
         pad_to = pad_to,
         distance = distance,
         format_time = format_time,
      },
      lume = lume,
   },

   time = function(ship)
      return ship.time_offset + (socket.gettime() - ship.load_time)
         * ship.time_factor
   end,

   find_by = function(ts, key, value)
      for _,t in ipairs(ts) do
         if(t[key] == value) then return t end
      end
   end,

   pad_to = pad_to,
   buffer = buffer,
}
