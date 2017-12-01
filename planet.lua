-- This is all about drawing planets with shaders and generating textures for
-- their surfaces.
-- Roughly based on code from https://github.com/meric/renderplanet/ except
-- for the texture parts.
if(not love.graphics) then return {} end
local lume = require("lume")
local texture = require("texture")
local template_texture = love.graphics.newImage("assets/texture_template.png",
                                                {linear=true})
local shader_source = love.filesystem.read("planet_shader.glsl")

local update = function(planet, body, dt)
   planet.x, planet.y = body.x, body.y
   if planet.rotate_retrograde then
      -- Rotate opposite direction as Earth
      planet.time = (planet.time + dt * planet.speed) % 2
   else
      -- Rotate same direction as Earth
      planet.time = (planet.time - dt * planet.speed) % 2
   end

   if(not planet.star) then
      planet.light_angle = math.atan2(-planet.y, planet.x)
      planet.planet_shader:send("light_angle", planet.light_angle)
   end

   planet.planet_shader:send("time", planet.time)
end

local make = function(options)
   local height = template_texture:getHeight()
   local planet = {
      x = options.x or 0,
      y = options.y or 0,
      planet_texture = options.texture or
         love.graphics.newImage("assets/" .. options.texture_name),
      time = 0,
      speed = (options.rot or 16) / 3600,
      rotate_angle = options.angle or 0,
      rotate_retrograde = options.retrograde or false,
      light_angle = options.light_angle or 0,
      size = height/2,
      radius = (options.star and 3048) or
         (options.texture_type == "gas" and options.mass * 2.3) or
         options.mass * 1.5,
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
   options.update = lume.fn(update, planet)
   return planet
end

local render_planet = function(planet)
   love.graphics.setShader(planet.planet_shader)
   love.graphics.draw(template_texture, 0, 0)
   love.graphics.setShader()
end

local draw = function(planet)
   love.graphics.push()
   love.graphics.translate(planet.x - planet.radius, planet.y - planet.radius)
   love.graphics.scale(planet.radius/planet.size)
   render_planet(planet)
   love.graphics.pop()
end

local seed_for = function(s)
   return tonumber(s:lower():gsub("[^a-z0-9]", ""), 36)
end

local random = function(body)
   local r = love.math.randomNormal
   love.math.setRandomSeed(seed_for(body.name))
   local radius = r(128, 512)
   if(body.texture_type == "gas") then radius = radius * 2 end
   local f = lume.fn(texture.generators[body.texture_type], r(256, 512))
   body.rot = body.rot or r(64, 64)
   body.angle = body.angle or math.rad(r(30, 45))
   body.texture = texture.random(radius,radius,f)
   love.math.setRandomSeed(love.timer.getTime())
   return make(body)
end

return {
   make=make,
   draw=draw,
   update=update,
   random=random,
}
