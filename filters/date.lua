--[[ Set the date if it is not already set
     and properly format it
--]]

local function split(s, delimiter)
    result = {}
    for match in (s..delimiter):gmatch("(.-)"..delimiter) do
        table.insert(result, match)
    end
    return result
end

function Meta(m)
  if m.date == nil then
    m.date = os.date("%e %B %Y")
  else
    local date = pandoc.utils.normalize_date(pandoc.utils.stringify(m.date))
    local year, month, day = table.unpack(split(date, '-'))
    m.date = os.date("%e %B %Y", os.time({year=year, month=month, day=day}))
  end
  return m
end
