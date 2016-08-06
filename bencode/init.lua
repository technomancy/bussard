local encode, decode

local function decode_list(str, t, total_len)
   -- print("list", str, lume.serialize(t))
   if(str:sub(1,1) == "e") then return t, total_len + 1 end
   local value, v_len = decode(str)
   table.insert(t, value)
   total_len = total_len + v_len
   return decode_list(str:sub(v_len + 1), t, total_len)
end

local function decode_table(str, t, total_len)
   -- print("table", str, lume.serialize(t))
   if(str:sub(1,1) == "e") then return t, total_len + 1 end
   local key, k_len = decode(str)
   local value, v_len = decode(str:sub(k_len+1))
   local end_pos = 1 + k_len + v_len
   t[key] = value
   total_len = total_len + k_len + v_len
   return decode_table(str:sub(end_pos), t, total_len)
end

function decode(str)
   -- print("decoding", str)
   if(str:sub(1,1) == "l") then
      return decode_list(str:sub(2), {}, 1)
   elseif(str:sub(1,1) == "d") then
      return decode_table(str:sub(2), {}, 1)
   elseif(str:sub(1,1) == "i") then
      return(tonumber(str:sub(2, str:find("e") - 1))), str:find("e")
   elseif(str:match("[0-9]+")) then
      local num_str = str:match("[0-9]+")
      local beginning_of_string = #num_str + 2
      local str_len = tonumber(num_str)
      local total_len = beginning_of_string + str_len - 1
      return str:sub(beginning_of_string, total_len), total_len
   else
      error("Could not parse "..str)
   end
end

local function encode_str(s) return #s .. ":" .. s end
local function encode_int(n) return "i" .. tostring(n) .. "e" end

local function encode_table(t)
   local s = "d"
   for k,v in pairs(t) do s = s .. encode(k) .. encode(v) end
   return s .. "e"
end

local function encode_list(l)
   local s = "l"
   for _,x in ipairs(l) do s = s .. encode(x) end
   return s .. "e"
end

function encode(x)
   if(type(x) == "table" and select("#", unpack(x)) == lume.count(x)) then
      return encode_list(x)
   elseif(type(x) == "table") then
      return encode_table(x)
   elseif(type(x) == "number" and math.floor(x) == x) then
      return encode_int(x)
   elseif(type(x) == "string") then
      return encode_str(x)
   else
      error("Could not encode " .. type(x) .. ": " .. tostring(x))
   end
end

return {decode=decode, encode=encode}
