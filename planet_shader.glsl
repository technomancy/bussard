// -*- c -*-
// Copyright © 2017 Eric Man
// Taken from https://github.com/meric/renderplanet/
extern Image planet_texture;
extern number time;
extern number rotate_angle;
extern number light_angle;
number M_PI = 3.1415926535897932384626433832795;

mat2 rotate2d(float _angle) {
  return mat2(cos(_angle), -sin(_angle),
              sin(_angle), cos(_angle));
}

bool has_rotate_matrix = false;

mat2 rotate_planet_matrix;
mat2 rotate_light_matrix;

vec4 effect( vec4 color, Image vectors, vec2 vectors_coords, vec2 screen_coords ){
  // Rotate planet
  if (!has_rotate_matrix) {
    rotate_planet_matrix = rotate2d(rotate_angle);
    rotate_light_matrix = rotate2d(light_angle + M_PI/4);
    has_rotate_matrix = true;
  }
  vec2 rotated_coords = rotate_planet_matrix * (vectors_coords-vec2(0.5));
  rotated_coords += vec2(0.5);

  vec4 vector = Texel(vectors, rotated_coords );

  if (distance(rotated_coords, vec2(0.5, 0.5)) > 0.5) {
    return vector;
  }

  // Retrieve planet texture pixel
  vec2 planet_coords;
  planet_coords.x = (vector.r + vector.g/255 + time)/2;
  planet_coords.y = vector.b + vector.a/255;

  if (planet_coords.x > 1) {
    planet_coords.x =  planet_coords.x - 1;
  }

  // Calculate shadow.
  vec2 light_coords = vec2(0, 0);
  vec2 shadow_coords = vectors_coords;

  shadow_coords -= vec2(0.5);
  light_coords -= vec2(0.5);
  light_coords = rotate_light_matrix * light_coords;
  number shadow = 0;
  shadow = 1-pow(distance(light_coords, shadow_coords)*0.9, 3);
  if (light_angle != 0 && shadow < 0.05) {
    shadow = 0.05;
  }

  vec4 pixel = Texel(planet_texture, planet_coords );

  %s

     return pixel;
}
