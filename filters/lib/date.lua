local fmt = string.format

local Date = setmetatable({
  __lt = function(self, b)
    return os.time(self) < os.time(b)
  end,

  __eq = function(a, b)
    return a.year == b.year and a.month == b.month and a.day == b.day
  end,

  __tostring = function(self)
    local delimiter = "."
    return fmt("%04d%s%02d%s%02d", self.year, delimiter, self.month, delimiter, self.day)
  end,

}, {
  __call = function(self, str)
    local date = pandoc.utils.normalize_date(pandoc.utils.stringify(str))
    local ya, ma, da = date:match("^(%d+)-(%d+)-(%d+)$")

    self.__index = self

    return setmetatable({
      day   = tonumber(da),
      month = tonumber(ma),
      year  = tonumber(ya),
    }, self)
  end,
})


local months = {
  "Jan",
  "Feb",
  "Mar",
  "Apr",
  "May",
  "Jun",
  "Jul",
  "Aug",
  "Sep",
  "Oct",
  "Nov",
  "Dec",
}

-- <pubDate> 23 May 2025 00:00:00 GMT</pubDate>
function Date.rfc822(self)
  month = months[self.month]
  return fmt("%02d %s %4d 00:00:00 GMT", self.day, month, self.year)
end

return Date
