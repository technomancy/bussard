-- functions for texture generation
local utils = require("utils")

local generators = {gas=
                       function(rr,x,y)
                          local r = love.math.noise(rr+2*x,rr+2*y)*100
                          local g = love.math.noise(rr+4*x,rr+4*y)*100
                          local b = love.math.noise(rr+8*x,rr+8*y)*100
                          return {r,g,b}
                       end,
                    grassy_oceany=
                       function(r,x,y)
                          local level = love.math.noise(r*x/128,r*y/128)
                          if(level > 0.5) then
                             return {20, 205*(1.5-level), 50}
                          else
                             return {0, 0, 100+300*level}
                          end
                       end,
                    star=
                       function(r,x,y)
                          local v = love.math.noise(r*x,r*y)*50
                          v = v + love.math.noise(2*r*x,2*r*y)*50
                          v = v + love.math.noise(4*r*x,4*r*y)*50
                          v = v + love.math.noise(8*r*x,8*r*y)*50
                          v = v + love.math.noise(16*r*x,16*r*y)*50
                          local s = love.math.noise(r*x,r*y)*255
                          return {utils.hsv(17,s,v)}
                       end,
                    -- rocky=function(r,x,y) end,
}

local random = function(w,h,f)
   local data = love.image.newImageData(w,h)
   for i = 0,w/2-1 do
      local x = i/w
      for j = 0,h-1 do
         local y = j/h
         data:setPixel(i,j,unpack(f(x,y)))
         -- Instead of doing something complicated to ensure the textures are
         -- consistent when X wraps, just take the first half and flip it along
         -- X to use it as the second half. No one will notice, riiiight?
         data:setPixel(w-i-1,j,unpack(f(x,y)))
      end
   end
   return love.graphics.newImage(data)
end

return {random=random, generators=generators}
