local core = require "filters.lib.core"

-- Store all document references
local refs = {}

function RawBlock(b)
  if b.format == "bibtex" or b.format == "biblatex" then
    local doc = pandoc.read(b.text, b.format)
    refs = core.concat(refs, doc.meta.references)
  end
end

function Meta(m)
  m.references = core.concat(m.references, refs)
  return m
end
