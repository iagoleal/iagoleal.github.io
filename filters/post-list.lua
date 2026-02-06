local Date = require "filters.lib.date"
local fs   = require "filters.lib.fs"
local core = require "filters.lib.core"
local map, take = core.map, core.take

local fmt = string.format

local function getfilename(path)
  local fname = pandoc.path.split_extension(pandoc.path.split_extension(pandoc.path.filename(path)))
  return fname
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

local function theme_marker(themes)
  local classes = {"theme-marker", table.unpack(themes)}

  return table.concat(classes, " ")
end

local function makeitem(meta)
  return fmt([[
    <li>
      <span class="%s" aria-hidden="true"></span>
      <a href="%s">
        <span class="title">%s</span>
        <time class="date" datetime="%s">%s</time>
      </a>
    </li>
    ]],
    theme_marker(meta.theme),
    meta.href,
    meta.title,
    meta.date:string("-"),
    meta.date:string(".")
  )
end

local function makelist(postlist)
  return fmt([[
    <ol class="post-list">
      %s
    </ol>
    ]],
    table.concat(postlist, "\n")
  )
end

local function make_postlist(path, n)
  local posts = pandoc.system.list_directory(path)
  n = math.min(n or math.huge, #posts)

  pandoc.log.info(fmt("Making post list with %d out of %d posts", n, #posts))


  local metadata = map(posts, function(post)
    pandoc.log.info("Post list: reading post " .. post)

    local meta = fs.read_metadata(pandoc.path.join{path, post})

    return {
      href       = fmt("/posts/%s/", getfilename(post)),
      date       = Date(meta.date),
      theme      = extract_strings(meta.theme),
      title      = pandoc.utils.stringify(meta.title),
      requisites = extract_strings(meta.requisites),
    }
  end)

  table.sort(metadata, function(a, b) return a.date > b.date end)

  local items = map(take(metadata, n), makeitem)

  return makelist(items)
end

function Block(elem)
  if elem.content then
    local text = pandoc.utils.stringify(elem.content)
    local num  = string.match(text, "^%s*{{%s*post%-list%s*(%d*)%s*}}%s*$")

    if num then
      local postlist = make_postlist("content/posts", tonumber(num))

      local div = pandoc.RawBlock("html", postlist)

      return div
    end
  end
end
