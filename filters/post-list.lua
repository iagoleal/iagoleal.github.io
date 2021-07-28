local fmt = string.format

local function trimspaces(s)
  return string.gsub(s, '^%s*(.-)%s*$', '%1')
end

local function getfilename(s)
  return string.match(s, "^.+/(.+)%..+$")
end

function split(s, delimiter)
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
        values[k] = trimspaces(matched)
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

local function makeitem(info)
  return pandoc.List {
    pandoc.Plain { -- This is a <li> 
      pandoc.Link ({
        pandoc.Span(fmt("%s: %s", info.title, info.subtitle), {class = "title"}),
        pandoc.Span(format_date(info.date), {class = "date"}),
      }, info.href)
    }
  }
end

local function make_postlist(path, n)
  local posts = scandir(path)
  local data = {}
  -- n = math.min(n, #postsagnas)
  for _, post in ipairs(posts) do
    print("TOC: reading post ", post)
    local info = readyaml(post, {"title", "subtitle", "date"})
    print("TOC: href ", info.href)
    local item = makeitem(info)
    table.insert(data, item)
  end
  return pandoc.BulletList(pandoc.List( data))
end

function Block(elem)
  if elem.content and elem.content[1].text == "{{post-list}}" then
    local postlist = make_postlist("content/posts")
    local div = pandoc.Div({postlist})
    div.classes = {"post-list"}
    return div
  else
    return elem
  end
end
