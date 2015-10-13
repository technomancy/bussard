return {
   seed = function(b, fs)
      local groups = love.filesystem.getDirectoryItems("data/news")
      b.os.fs.mkdir(fs, "/usr/news")
      for _,group in ipairs(groups) do
         fs.usr.news[group] = nil
         b.os.fs.mkdir(fs, "/usr/news/" .. group)
         local msgs = love.filesystem.getDirectoryItems("data/news/" .. group)
         for _,name in ipairs(msgs) do
            local filename = "data/news/" .. group .. "/" .. name
            fs.usr.news[group][name] = love.filesystem.read(filename)
         end
      end
   end
}
