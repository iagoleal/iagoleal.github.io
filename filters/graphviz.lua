function CodeBlock(block)
  if block.classes[1] == "dot" then
    local svg = pandoc.pipe("dot", {"-Tsvg"}, block.text)
    block.text = svg
    return pandoc.RawBlock("html", svg)
  end
end
