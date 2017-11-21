-- This is all about drawing planets with shaders and generating textures for
-- their surfaces.
-- Roughly based on code from https://github.com/meric/renderplanet/ except
-- for the texture parts.
if(not love.graphics) then return {} end
local template_texture = love.graphics.newImage("assets/texture_template.png",
                                                {linear=true})
local shader_source = love.filesystem.read("planet_shader.glsl")

local render_arc = function(planet, a, b)
   love.graphics.arc("line", "open", planet.size, planet.size, planet.size,
                        -planet.light_angle + a, -planet.light_angle + b)
end

local set_atmosphere_color = function(planet, a)
   love.graphics.setColor(
      planet.atmosphere_color[1],
      planet.atmosphere_color[2],
      planet.atmosphere_color[3], a or 255)
end

local prerender_atmosphere = function(planet)
   love.graphics.setLineStyle("smooth")
   love.graphics.setLineWidth(16)
   local n = planet.atmosphere_size
   local tail = math.pi/6 -- how long is section of atmosphere that tapers off
   local size = 0.6 -- how big is shadow of atmosphere
   local tau = 2 * math.pi
   for i = n + 3, 3, -1 do
      love.graphics.setLineWidth(i)
      local step = (i - 3) / n
      set_atmosphere_color(planet, 5)
      render_arc(planet, size + tail * step - tau, -(size + tail * step))
      set_atmosphere_color(planet, 255 * step)
      love.graphics.setLineWidth(1)
      render_arc(planet, size + tail * step - tau,
                 size + tail * (step + 1/n) - tau)
      render_arc(planet, -(tail * step + size), -(size + tail * (step + 1/n)))
   end
   set_atmosphere_color(planet, 255)
   love.graphics.setLineWidth(1)
   render_arc(planet, tail + size - tau, -(tail + size))
end

local make = function(options)
   local height = template_texture:getHeight()
   local planet = {
      x = options.x or 0,
      y = options.y or 0,
      planet_texture = options.planet_texture,
      clouds_texture = options.clouds_texture,
      template_texture = template_texture,
      time = 0,
      speed = options.speed or 0.1,
      rotate_angle = options.rotate_angle or 0,
      rotate_retrograde = options.retrograde or false,
      light_angle = options.light_angle or 0,
      size = height/2,
      radius = options.radius or height/2,
      atmosphere_color = options.atmosphere_color or {160, 160, 165},
      atmosphere_size = options.atmosphere_size or 24,
   }
   local planet_shader_source = shader_source:format[[
        pixel.r *= shadow;
        pixel.g *= shadow;
        pixel.b *= shadow;
      ]]
   planet.planet_shader = love.graphics.newShader(planet_shader_source)
   planet.planet_shader:send("planet_texture", planet.planet_texture)
   planet.planet_shader:send("light_angle", planet.light_angle)
   planet.planet_shader:send("rotate_angle", planet.rotate_angle)
   if planet.clouds_texture then
      local clouds_shader_source = shader_source:format[[
        pixel.r = 1-pixel.r;
        pixel.g = 1-pixel.g;
        pixel.b = 1-pixel.b;
        pixel.a = pixel.r * shadow;
      ]]
      planet.clouds_shader = love.graphics.newShader(clouds_shader_source)
      planet.clouds_shader:send("planet_texture", planet.clouds_texture)
      planet.clouds_shader:send("light_angle", planet.light_angle)
      planet.clouds_shader:send("rotate_angle", planet.rotate_angle)
   end
   planet.atmosphere = love.graphics.newCanvas(
      height + 2 * planet.atmosphere_size,
      height + 2 * planet.atmosphere_size)
   love.graphics.setCanvas(planet.atmosphere)
   love.graphics.clear()
   love.graphics.setBlendMode("alpha")
   love.graphics.push()
   love.graphics.translate(planet.atmosphere_size, planet.atmosphere_size)
   prerender_atmosphere(planet)
   love.graphics.pop()
   love.graphics.setCanvas()
   return planet
end

local update = function(planet, dt)
   if planet.rotate_retrograde then
      -- Rotate opposite direction as Earth
      planet.time = (planet.time + dt * planet.speed) % 2
   else
      -- Rotate same direction as Earth
      planet.time = (planet.time - dt * planet.speed) % 2
   end

   planet.light_angle = math.atan2(-planet.y, planet.x)
   planet.planet_shader:send("light_angle", planet.light_angle)
   if(planet.clouds_shader) then
      planet.clouds_shader:send("light_angle", planet.light_angle)
   end

   planet.planet_shader:send("time", planet.time)
   if planet.clouds_shader then
      planet.clouds_shader:send("time", planet.time)
   end
end

local render_template = function(planet)
   love.graphics.draw(planet.template_texture, 0, 0)
end

local render_planet = function(planet)
   love.graphics.setShader(planet.planet_shader)
   render_template(planet)
   love.graphics.setShader()
end

local render_clouds = function(planet)
   if planet.clouds_shader then
      love.graphics.setShader(planet.clouds_shader)
      render_template(planet)
      love.graphics.setShader()
   end
end

local render_atmosphere = function(planet)
   love.graphics.setColor(255, 255, 255, 255)
   love.graphics.setBlendMode("alpha", "premultiplied")
   love.graphics.draw(planet.atmosphere,
                         -planet.atmosphere_size,
                         -planet.atmosphere_size)
   love.graphics.setBlendMode("alpha")
end

local draw = function(planet)
   love.graphics.push()
   love.graphics.translate(planet.x - planet.radius, planet.y - planet.radius)
   love.graphics.scale(planet.radius/planet.size)
   render_planet(planet)
   render_clouds(planet)
   render_atmosphere(planet)
   love.graphics.pop()
end

-- TODO: other texture generators
local texture_fns = {gas=function(x,y)
                        local r = love.math.noise(2*x,2*y)*100
                        local g = love.math.noise(4*x,4*y)*100
                        local b = love.math.noise(8*x,8*y)*100
                        return {r,g,b}
                    end}

local random_texture = function(w,h,f)
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

local seed_for = function(s)
   return tonumber(s:lower():gsub("[^a-z0-9]", ""), 36)
end

local random = function(name, draw_type)
   local r = love.math.randomNormal
   love.math.setRandomSeed(seed_for(name))
   local radius = r(128, 512)
   local atmo = {r(50,160), r(50,160), r(50,160)}
   local f = texture_fns[draw_type]
   local new_planet = make({radius=radius, speed=r(0.1, 0.2),
                            planet_texture=random_texture(radius,radius,f),
                            atmosphere_color=atmo, atmosphere_size=r(16,32)})
   love.math.setRandomSeed(love.timer.getTime())
   return new_planet
end

return {
   make=make,
   draw=draw,
   update=update,
   random=random,
}
