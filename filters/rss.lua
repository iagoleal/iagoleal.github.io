local Date = require "filters.lib.date"
local fs   = require "filters.lib.fs"
local core = require "filters.lib.core"

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
    title           = pandoc.utils.stringify(meta.title),
    date            = date,
    ["date-rfc822"] = date:rfc822(),
    description     = pandoc.utils.stringify(meta.description),
    url             = canonical_url(post),
  }

  return info
end

function Meta(m)
  local postdir = "content/posts"
  local posts   = pandoc.system.list_directory(postdir)

  m.posts = core.map(posts, function(post)
    return makeitem(pandoc.path.join{postdir, post})
  end)

  table.sort(m.posts, function(a, b) return a.date > b.date end)

  return m
end
