local function postfile(fname)
  return string.match(fname, "^.*/(.*)/index.html$")
end

local function template_variable(k)
  return PANDOC_WRITER_OPTIONS.variables[k]
end

function Meta(m)
  if m.url == nil then
    local fname = PANDOC_STATE.output_file
    local sitebase = template_variable("site")

    m.url = pandoc.path.join {sitebase, "posts", postfile(fname)}
  else
    error "Non-canonical url set on metadata"
  end

  return m
end
