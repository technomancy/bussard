require("luarocks.loader")
local lunatest = require("lunatest")

for _,filename in ipairs(love.filesystem.getDirectoryItems("tests")) do
   if(filename:match("^test_.*%.lua$")) then
      lunatest.suite("tests." .. filename:gsub(".lua", ""))
   end
end

lunatest.run(nil, {"--verbose"})
love.event.quit()
