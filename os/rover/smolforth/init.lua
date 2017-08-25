#!/usr/bin/env lua
local lume = require("lume")

local stack_fn = function(arg_count, f)
   return function(stack)
      local args = lume.last(stack, arg_count)
      for _=1,arg_count do table.remove(stack) end
      lume.push(stack, f(unpack(args)))
   end
end

local pop_token = function(env)
   local token, w = env.input:match("^(%S+)(%s*)")
   if(token) then env.input = env.input:sub(#token+#w+1) end
   return token
end

local function read_till(env, stoppers, recurse_pairs)
   local token = nil
   while not lume.find(stoppers, token) do
      token = pop_token(env)
      if(recurse_pairs and recurse_pairs[token]) then
         read_till(env, {recurse_pairs[token]}, recurse_pairs)
      end
   end
   return token
end

local function eval_token(env, token)
   if(token:match("^([.0-9]+)$")) then
      table.insert(env.stack, tonumber(token))
   elseif(token == "true") then table.insert(env.stack, true)
   elseif(token == "false") then table.insert(env.stack, false)
   elseif(type(env.dictionary[token]) == "function") then
      env.dictionary[token](env.stack, env.dictionary, env)
   elseif(type(env.dictionary[token]) == "table") then
      lume.map(env.dictionary[token], lume.fn(eval_token, env))
   else
      env.write("unknown word: " .. token .. "\n")
   end
end

local function eval(env, stop_at, also_stop_at)
   local token = pop_token(env)
   if(token == stop_at or token == also_stop_at) then return token end
   eval_token(env, token)
   return eval(env, stop_at, also_stop_at)
end

local primitives = {
   swap = stack_fn(2, function(a, b) return b, a end),
   rot = stack_fn(3, function(a, b, c) return b, c, a end),
   dup = stack_fn(1, function(a) return a, a end),
   over = stack_fn(2, function(a, b) return a, b, a end),

   ["+"] = stack_fn(2, function(a, b) return a + b end),
   ["-"] = stack_fn(2, function(a, b) return a - b end),
   ["*"] = stack_fn(2, function(a, b) return a * b end),
   ["/"] = stack_fn(2, function(a, b) return a / b end),

   ["="] = stack_fn(2, function(a, b) return a == b end),
   [">"] = stack_fn(2, function(a, b) return a > b end),
   ["<"] = stack_fn(2, function(a, b) return a < b end),

   ["not"] = stack_fn(1, function(a) return not a end),
   ["or"] = stack_fn(2, function(a, b) return a or b end),
   ["and"] = stack_fn(2, function(a, b) return a and b end),

   ["if"] = function(stack, _, env)
      if(table.remove(stack)) then
         if(eval(env, "then", "else") == "else") then
            read_till(env, {"then"}, {["if"]="then"})
         end
      else
         if(read_till(env, {"else", "then"}, {["if"]="then"}) == "else") then
            eval(env, "then")
         end
      end
   end,

   [":"] = function(_, dictionary, env)
      local name, token = pop_token(env), nil
      dictionary[name] = {}
      while token ~= ";" do
         table.insert(dictionary[name], token)
         token = pop_token(env)
      end
   end,

   ["("] = function(_, _, env) read_till(env, {")"}) end,
   [".s"] = function(stack, _, env)
      env.write(table.concat(lume.map(stack, tostring), " ") .. "\n")
   end,
}

local make_env = function(read, write, extra_fns, bootstrap_path)
   local env = {stack={},dictionary=lume.extend({}, primitives, extra_fns),
                read=read, write=write,}
   for line in io.lines(bootstrap_path or "smolforth.fs") do
      env.input = line
      eval(env)
   end
   return env
end

local function repl(env)
   env.input = env.read()
   if(not env.input) then return env end
   eval(env)
   return repl(env)
end

if(#{...} == 0) then repl(make_env(io.read, io.write)) end

return {make_env=make_env, eval=eval, repl=repl, stack_fn=stack_fn}
