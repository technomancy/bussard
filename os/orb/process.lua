orb.process = {
   -- Create a coroutine for a command to run inside and place it into the
   -- process table. The process table is stored in the filesystem under
   -- f.proc[user]
   spawn = function(f, env, command, extra_sandbox)
      local co = coroutine.create(function()
            orb.shell.exec(f, env, "smash", extra_sandbox) end)
      local id = orb.process.id_for(co)
      f.proc[env.USER][id] = { thread = co,
                               command = command,
                               id = id,
                               _user = env.USER,
      }
      return co, id
   end,

   -- The process ID is taken from lua's own tostring called on a coroutine.
   id_for = function(p)
      return tostring(p):match(": 0x(.+)")
   end,

   -- Loop through all the coroutines in the process table and give them all
   -- a chance to run till they yield. No attempt at fairness or time limits
   -- yet.
   scheduler = function(f)
      for _,procs in pairs(f.proc) do
         if(type(procs) == "table") then
            for k,p in pairs(procs) do
               if(type(p) == "table" and p.thread) then
                  if(coroutine.status(p.thread) == "dead") then
                     procs[k] = nil
                  else
                     coroutine.resume(p.thread)
                  end
               end
            end
         end
      end
   end,
}
