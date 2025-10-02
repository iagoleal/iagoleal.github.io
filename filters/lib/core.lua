local M = {}

function M.map(t, f)
  out = {}
  for k, v in pairs(t) do
    out[k] = f(v)
  end
  return out
end

function M.take(t, n)
  out = {}
  for i=1, n do
    out[i] = t[i]
  end
  return out
end

function M.concat(...)
  local out = {}

  for i = 1, select("#", ...) do
    local t = select(i, ...)
    if type(t) ~= "nil" then
      for _, v in ipairs(t) do
        out[#out + 1] = v
      end
    end
  end
  return out
end

return M
