love.graphics.half_hyperbola = function(style, x, y, a, b, segments)
  segments = segments or 36
  local step = 1/math.sqrt(segments*math.abs(a/b))
  local ps = {}
  for i = -segments, segments do
    local t = step*i
    table.insert(ps, x+a*math.cosh(t))
    table.insert(ps, y+b*math.sinh(t))
  end
  love.graphics.polygon(style, ps)
end

return {
}
