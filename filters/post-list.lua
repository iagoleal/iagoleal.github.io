local Date = require "filters.lib.date"
local fs   = require "filters.lib.fs"
local core = require "filters.lib.core"
local map, take = core.map, core.take

local fmt = string.format

local function getfilename(path)
  local fname = pandoc.path.split_extension(pandoc.path.split_extension(pandoc.path.filename(path)))
  return fname
end

local function theme_marker(themes)
  local classes = {"theme-marker", table.unpack(themes)}

  return pandoc.RawInline("html", string.format(
    '<span class="%s"></span>', table.concat(classes, " ")))
end

local function makeitem(meta)
  return pandoc.List {
    pandoc.Plain {
      theme_marker(meta.theme),
      pandoc.Link({
        pandoc.Span(meta.title, {class = "title"}),
        pandoc.Span(tostring(meta.date), {class = "date"})
      }, meta.href)
    }
  }
end

--- Extracts a list of themes as strings from a pandoc List or single element.
--- @param xs pandoc.List|string A pandoc List of themes or a single theme
--- @return string[] List of theme names as strings
local function extract_strings(xs)
  if pandoc.utils.type(xs) ~= "List" then
    xs = pandoc.List({xs})
  end

  return xs:map(pandoc.utils.stringify)
end

local function make_postlist(path, n)
  local posts = pandoc.system.list_directory(path)
  n = math.min(n or math.huge, #posts)

  pandoc.log.info(fmt("Making post list with %d out of %d posts", n, #posts))


  local metadata = map(posts, function(post)
    local meta = fs.read_metadata(pandoc.path.join{path, post})

    pandoc.log.info("Post list: reading post " .. post)

    meta.href       = fmt("/posts/%s/", getfilename(post))
    meta.date       = Date(meta.date)
    meta.theme      = extract_strings(meta.theme)
    meta.requisites = extract_strings(meta.requisites)

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
