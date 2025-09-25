local Date = require "filters.lib.date"
local fs   = require "filters.lib.fs"

local function map(t, f)
  out = {}
  for k, v in pairs(t) do
    out[k] = f(v)
  end

  return out
end

local template_feed = pandoc.template.compile(pandoc.template.get("templates/rss.xml"))
local template_item = pandoc.template.compile(pandoc.template.get("templates/rss-item.xml"))

function template_variable(k)
  return tostring(PANDOC_WRITER_OPTIONS.variables[k])
end

local function getfilename(path)
  local fname = pandoc.path.split_extension(pandoc.path.split_extension(pandoc.path.filename(path)))
  return fname
end

function canonical_url(fname)
  local relative = getfilename(fname)
  local sitebase = template_variable("site-url")

  return pandoc.path.join {sitebase, "posts", relative} .. "/"
end

function makeitem(post)
  local meta = fs.read_metadata(post)
  local date = Date(meta.date)

  local info = {
    title = pandoc.utils.stringify(meta.title),
    date  = date:rfc822(),
    description = pandoc.utils.stringify(meta.description),
    url  = canonical_url(post),
    ["author-meta"] = template_variable("author-meta")
  }

  return info
end

function Meta(m)
  local postdir = "content/posts"
  local posts   = pandoc.system.list_directory(postdir)

  metadata = map(posts, function(post)
    return makeitem(pandoc.path.join{postdir, post})
  end)

  table.sort(metadata, function(a, b) return a.date > b.date end)

  m.items = map(metadata, function(info)
    local content = pandoc.template.apply(template_item, info)
    return tostring(content)
  end)

  return m
end
