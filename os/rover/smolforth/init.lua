local lume = require("lume")

-- helper to turn Lua functions into Forth-like words take/return on stack
local stack_fn = function(arg_count, f)
   return function(stack)
      assert(#stack >= arg_count, "stack underflow")
      local args = lume.last(stack, arg_count)
      for _=1,arg_count do table.remove(stack) end
      lume.push(stack, f(unpack(args)))
   end
end

local pop_token = function(env)
   local token, w = env.input:match("^(\"[^\"]+\")(%s*)")
   if(not token) then token, w = env.input:match("^(%S+)(%s*)") end
   if(token) then env.input = env.input:sub(#token+#w+1) end
   return token
end

local function read_till(env, stoppers)
   local token = nil
   while not lume.find(stoppers, token) do token = pop_token(env) end
   return token
end

local function compile_token(token, env, exec_token)
   if(not env.compiling) then return exec_token(env, token)
   elseif(env.compilers[token]) then env.compilers[token](env.stack, env, token)
   else table.insert(env.compiling, token) end
end

local function exec_word(body, env, exec_token)
   table.insert(env.pos, 1)
   while body[lume.last(env.pos)] do
      local pos = lume.last(env.pos)
      if(#env.conditionals == 0) then
         exec_token(env, body[pos])
      elseif(body[pos] == "then") then
         assert(#env.conditionals > 0, "if/then mismatch")
         table.remove(env.conditionals)
      elseif(body[pos] == "else") then
         assert(#env.conditionals > 0, "if/else mismatch")
         env.conditionals[#env.conditionals] =
            not env.conditionals[#env.conditionals]
      elseif(lume.last(env.conditionals)) then
         exec_token(env, body[pos])
      elseif(body[pos] == "if") then
         local word = body[pos]
         while(word and word ~= "then") do
            env.pos[#env.pos] = env.pos[#env.pos] + 1
            word = body[env.pos[#env.pos]]
         end
      end
      env.pos[#env.pos] = env.pos[#env.pos] + 1
   end
   table.remove(env.pos)
end

local function exec_token(env, token)
   if(env.compiling) then return compile_token(token, env, exec_token) end
   if(token:match("^([0-9]+[.0-9]*)$")) then
      table.insert(env.stack, tonumber(token))
   elseif(token:match("^\".*\"$")) then
      table.insert(env.stack, token:sub(2, -2))
   elseif(token == "true") then table.insert(env.stack, true)
   elseif(token == "false") then table.insert(env.stack, false)
   elseif(type(env.dictionary[token]) == "function") then
      env.dictionary[token](env.stack, env)
   elseif(type(env.dictionary[token]) == "table") then
      exec_word(env.dictionary[token], env, exec_token)
   else
      error("unknown word: " .. token .. "\n")
   end
end

local function exec(env, stoppers)
   local token = pop_token(env)
   if(not token or lume.find(stoppers or {}, token)) then return token end
   exec_token(env, token)
   return exec(env, stoppers)
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
   ["%"] = stack_fn(2, function(a, b) return a % b end),

   ["="] = stack_fn(2, function(a, b) return a == b end),
   [">"] = stack_fn(2, function(a, b) return a > b end),
   ["<"] = stack_fn(2, function(a, b) return a < b end),

   ["not"] = stack_fn(1, function(a) return not a end),
   ["or"] = stack_fn(2, function(a, b) return a or b end),
   ["and"] = stack_fn(2, function(a, b) return a and b end),

   [".."] = stack_fn(2, function(a, b) return a .. b end),

   [":"] = function(_, env)
      env.compiling = {}
      env.dictionary[pop_token(env)] = env.compiling
   end,
   ["if"] = function(stack, env)
      assert(#stack >= 1, "stack underflow")
      table.insert(env.conditionals, table.remove(stack))
   end,
   ["begin"] = function(_, env)
      table.insert(env.loops, {start=lume.last(env.pos)})
   end,
   ["until"] = function(stack, env)
      assert(#env.loops >= 1, "begin/until mismatch")
      if(table.remove(stack)) then
         table.remove(env.loops)
      else
         env.pos[#env.pos] = lume.last(env.loops).start
      end
   end,
   ["do"] = function(stack, env)
      assert(#stack >= 2, "stack underflow")
      local index = table.remove(env.stack)
      table.insert(env.loops, {index=index, limit=table.remove(env.stack),
                               start=lume.last(env.pos)})
   end,
   ["i"] = function(stack, env)
      assert(#env.loops >= 1, "i outside loop")
      table.insert(stack, lume.last(env.loops).index)
   end,
   ["loop"] = function(_, env)
      assert(#env.loops >= 1, "do/loop mismatch")
      local loop = lume.last(env.loops)
      assert(loop.index and loop.limit, "do/loop and begin/until mismatch")
      if(loop.index < loop.limit) then
         loop.index = loop.index + 1
         env.pos[#env.pos] = loop.start
      else
         table.remove(env.loops)
      end
   end,
   ["("] = function(_,  env) read_till(env, {")"}) end,
   [".s"] = function(stack, env)
      env.write(table.concat(lume.map(stack, tostring), " ") .. "\n")
   end,
}

local compilers = {
   [";"] = function(_, env) env.compiling = false end,
   ["("] = function(_,  env) read_till(env, {")"}) end,
}

local make_env = function(read, write, extra_entries, bootstrap_path)
   local env = {stack={}, read=read, write=write, compiling=false,
                dictionary=lume.extend({}, primitives, extra_entries),
                compilers=lume.extend({}, compilers),
                conditionals={}, loops={}, pos={}}
   for line in io.lines(bootstrap_path or "smolforth.fs") do
      env.input = line
      exec(env)
   end
   return env
end

local function repl(env)
   env.input = env.read()
   if(not env.input) then return env end
   local ok, err = pcall(exec, env)
   env.write((ok and "ok" or err) .. "\n")
   return repl(env)
end

return {make_env=make_env, exec=exec, repl=repl, stack_fn=stack_fn}
