local fmt = string.format

local function map(t, f)
  out = {}
  for k, v in pairs(t) do
    out[k] = f(v)
  end

  return out
end

local function take(t, n)
  out = {}

  for i=1,n do
    out[i] = t[i]
  end

  return out

end

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
  end
}, {
  __call = function(self, str)
    local date = pandoc.utils.normalize_date(pandoc.utils.stringify(str))
    local ya, ma, da = date:match("^(%d+)-(%d+)-(%d+)$")

    return setmetatable({
      day   = tonumber(da),
      month = tonumber(ma),
      year  = tonumber(ya),
    }, self)
  end,
})

local function getfilename(path)
  local fname = pandoc.path.split_extension(pandoc.path.filename(path))
  return fname
end

function readfile(fname, mode)
  local f = io.open(fname, "r")

  if not f then
    io.stderr:write(fmt("ERROR reading file %s\n", fname))
    return nil
  end

  local content = f:read(mode)

  f:close()

  return content
end

function read_metadata(fname)
  -- Read the entire file content
  local content = readfile(fname, "*a")
  local doc = pandoc.read(content, "markdown")

  return doc.meta  -- We only need the metadata
end

local function makeitem(meta)
  return pandoc.List {
    pandoc.Plain { -- This is a <li>
      pandoc.Link ({
        pandoc.Span(meta.title, {class = "title"}),
        pandoc.Span(tostring(meta.date), {class = "date"}),
      }, meta.href)
    }
  }
end

local function make_postlist(path, n)
  local posts = pandoc.system.list_directory(path)
  n = math.min(n or math.huge, #posts)

  pandoc.log.info(fmt("Making post list with %d out of %d posts", n, #posts))

  local metadata = map(posts, function(post)
    local meta = read_metadata(pandoc.path.join{path, post})

    pandoc.log.info("Post list: reading post " .. post)

    meta.href = fmt("/posts/%s/", getfilename(post))
    meta.date = Date(meta.date)

    return meta
  end)

  table.sort(metadata, function(a, b) return a.date > b.date end)

  return map(take(metadata, n), makeitem)
end

function Block(elem)
  if elem.content then
    local text = pandoc.utils.stringify(elem.content)
    local num  = string.match(text, "^%s*{{%s*post%-list%s*(%d*)%s*}}%s*$")

    if num then
      local postlist = make_postlist("content/posts", tonumber(num))

      local div = pandoc.Div({
        pandoc.BulletList(postlist)
      }, {
        class = "post-list",
      })

      return div
    end
  end
end
