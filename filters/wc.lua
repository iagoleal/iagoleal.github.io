--[[ Count words and estimate reading time

    Remove markup and add values to variables.
--]]

words = 0
wpm = 275.0

wordcount = {
  Str = function(el)
    -- We don't count a word if it's entirely punctuation:
    if el.text:match("%P") then
      words = words + 1
    end
  end,
  Code = function(el)
    _,n = el.text:gsub("%S+","")
    words = words + n
  end,
  CodeBlock = function(el)
    _,n = el.text:gsub("%S+","")
    words = words + n
  end,
  Math = function(el)
    _,n = el.text:gsub("%S+", "")
    words = words + n
  end
}

function assign_counts(meta)
  meta.wordcount=string.format("%s", words)
  local expected_mins = words / wpm
  if expected_mins < 60 then
    local hours = expected_mins / 60
    local minutes = expected_mins % 60
    meta.expectedtime = string.format("%.0fh%.0f", hours, minutes)
  else
    local minutes = expected_mins
    meta.expectedtime = string.format("%.0f min", minutes)
  end
  return meta
end

function counter(el)
  pandoc.walk_block(pandoc.Div(el.blocks), wordcount)
  return el
end

return {{Pandoc = counter },
    {Meta = assign_counts}}
