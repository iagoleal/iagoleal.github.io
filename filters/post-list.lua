local fmt = string.format

local function trimspaces(s)
  return string.gsub(s, '^%s*(.-)%s*$', '%1')
end

local function getfilename(s)
  return string.match(s, "^.+/(.+)%..+$")
end

local function split(s, delimiter)
    result = {}
    for match in (s..delimiter):gmatch("(.-)"..delimiter) do
        table.insert(result, match)
    end
    return result
end
-- Lua implementation of PHP scandir function
local function scandir(directory)
    local t = {}
    local file = io.popen(fmt("find '%s' -maxdepth 1 -type f", directory))
    for filename in file:lines() do
      table.insert(t, filename)
    end
    file:close()
    return t
end

-- Iterate over the lines of a file and read the YAML fields into a table.
-- NOTE: The first match for the field is kept.
local function readyaml(fname, keys)
  local values = { href = fmt("posts/%s/", getfilename(fname))}
  for line in io.lines(fname) do
    for i = #keys, 1, -1 do
      local k = keys[i]
      local matched = string.match(line, '^' .. k .. ':%s+(.+)%s*$')
      if matched then
        if k == 'date' then
          values[k] = pandoc.utils.normalize_date(matched)
        else
          values[k] = trimspaces(matched)
        end
        table.remove(keys, i)
        break
      end
    end
    if #keys == 0 then
      break
    end
  end
  return values
end

local function format_date(date)
  return string.gsub(date, '-', '.')
end

local function format_title(title, subtitle)
  if subtitle then
    return fmt("%s: %s", title, subtitle)
  else
    return title
  end
end

local function makeitem(info)
  return pandoc.List {
    pandoc.Plain { -- This is a <li>
      pandoc.Link ({
        pandoc.Span(format_title(info.title, info.subtitle), {class = "title"}),
        pandoc.Span(format_date(info.date), {class = "date"}),
      }, info.href)
    }
  }
end

local function compare_dates(a, b)
  local ya, ma, da = table.unpack(split(a.date, '-'))
  local yb, mb, db = table.unpack(split(b.date, '-'))
  return os.time({year = ya, month = ma, day = da})
       > os.time({year = yb, month = mb, day = db})
end

local function make_postlist(path, n)
  local posts = scandir(path)
  local data = {}
  local headlines = {}
  n = n and math.min(n, #posts) or #posts
  print(fmt("Making post list with %d of %d posts", n, #posts))
  for _, post in ipairs(posts) do
    print("\tPost list: reading post " .. post)
    local info = readyaml(post, {"title", "subtitle", "date"})
    table.insert(data, info)
  end
  table.sort(data, compare_dates)
  for i = 1, n do
    headlines[i] = makeitem(data[i])
  end
  return pandoc.BulletList(headlines)
end

function Block(elem)
  if elem.content then
    local text = pandoc.utils.stringify(elem.content)
    local num = string.match(text, "^%s*{{%s*post%-list%s*(%d*)%s*}}%s*$")
    if num then
      local postlist = make_postlist("content/posts", tonumber(num))
      local div = pandoc.Div({postlist}, {class = "post-list"})
      return div
    end
  end
end
