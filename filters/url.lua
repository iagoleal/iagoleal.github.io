local function postfile(fname)
  return string.match(fname, "^.*/(.*)/index.html$")
end

local function template_variable(k)
  return tostring(PANDOC_WRITER_OPTIONS.variables[k])
end

-- Encode a URL using percent notation
-- https://en.wikipedia.org/wiki/Percent-encoding
function percent_encode(s)
  local encoded = string.gsub(s, "([^%w%-%_%.%~])",
    function(c)
      return string.format("%%%02X", string.byte(c))
    end
  )

  return encoded
end


function Meta(m)
  if m.url == nil then
    local fname    = postfile(PANDOC_STATE.output_file)
    local sitebase = template_variable("site-url")

    m.url = pandoc.path.join {sitebase, "posts", fname}
  else
    error "Non-canonical url set on metadata"
  end

  return m
end
