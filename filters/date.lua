--[[ Set the date if it is not already set
     and properly format it
--]]

function collect(iter)
  local t = {}
  for k in iter do
    table.insert(t, k)
  end
  return t
end

function Meta(m)
  if m.date == nil then
    m.date = os.date("%e %B %Y")
  else
    for k, v in pairs(m.date) do
      if type(v) == "table" and v.t == 'Str' then
        local matches = collect(v.text:gmatch("%d*"))
        if #matches == 3 then
          local date_t = {year = matches[1],
                          month = matches[2],
                          day = matches[3]}
          v.text = os.date("%e %B %Y", os.time(date_t))
        end
      end
    end
  end
  return m
end
