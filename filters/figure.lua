--[[
    CodeBlock -> Image filter

  This filter compiles fenced code blocks describing figures
  and substitutes them in the output html page.

  Currently supported block classes:
  * tikz
  * tikzcd
  * dot (graphviz)

Note: Tex code blocks use any environment previously declared as raw tex blocks.
--]]
local path = require "pandoc.path"

local fmt = string.format

local illustrators = {}

-----------------------------
-- Shell-like functionality
-----------------------------

local function mkparent(fname)
  return pandoc.system.make_directory(path.directory(fname), true)
end

-- Check if file exists.
local function file_exists(fname)
  local f = io.open(fname, "r")
  if f ~= nil then
    io.close(f)
    return true
  else
    return false
  end
end

-- Copy contents from a file to another, creating any directory needed in the process.
local function copy(source, target)
  mkparent(target)
  os.execute(fmt("cp --update --no-target-directory %s %s", source , target))
end

-------------------------
-- Output Tag Template
-------------------------

local template_tag = [[
<object data="%s" type="image/svg+xml">
  Browser lacks SVG support.
</object>]]

local function format_tag(fname, block)
  return pandoc.Figure(
     pandoc.RawBlock("html", template_tag:format(fname, fname))
    , {}
    , block.attr
    )
end

-------------------------
-- Graphviz Illustrator
-------------------------

illustrators.graphviz = {
  match = function(class)
    return class == "dot"
  end,

  format = function(block)
    return block.text
  end,

  -- Turn a string in Graphviz DOT language into an SVG image.
  convert = function(target, code)
    return pandoc.pipe("dot", {"-Tsvg", "-o", target}, code)
  end,

  ext = "dot",
}

-------------------------
-- Tikz Illustrator
-------------------------

local template_tikz = [[
\documentclass{standalone}

\def\pgfsysdriver{pgfsys-dvisvgm.def}

\usepackage{xcolor,lmodern,amsfonts,tikz}

%% Additional packages: usepackage
\usepackage{%s}

%% Libraries for tikz: tikzlibrary
\usetikzlibrary{scopes,quotes,positioning,matrix,calc,arrows.meta}
\usetikzlibrary{%s}

%% Raw TeX blocks
%s

%% Additional preamble; loaded after tikz
%s

\begin{document}
\nopagecolor
\begin{%s}
%s
\end{%s}
\end{document}
]]

-- State for remembering all raw tex code
-- that comes before the code block in the post.
local tex_snippets = {}

illustrators.tikz = {
  match = function(class)
    return class == "tikz" or class == "tikzcd"
  end,

  format = function(block)
    local pkgs     = block.attributes["usepackage"]  or ""
    local libs     = block.attributes["tikzlibrary"] or ""
    local gdlibs   = block.attributes["gdlibrary"]   or ""
    local preamble = block.attributes["preamble"]    or ""
    local rawtex   = table.concat(tex_snippets, "\n\n")

    local env
    if     block.classes[1] == "tikz" then
      env = "tikzpicture"
    elseif block.classes[1] == "tikzcd" then
      pkgs = pkgs .. ",tikz-cd"
      env = "tikzcd"
    end

    return template_tikz:format(pkgs, libs, rawtex, preamble, env, block.text, env)
  end,

  -- Turn a string containing Latex code into a SVG image.
  convert = function(target, code)
    return pandoc.pipe("scripts/tex2svg", {"-", target}, code)
  end,

  -- Store all raw blocks of tex code.
  -- These will be inserted as part of the preamble for all figures.
  snippet = function(raw_block)
    if raw_block.format == "tex" then
      table.insert(tex_snippets, raw_block.text)
    end
  end,

  ext = "tikz",
}

------------------------------------------
-- Actual figure making
------------------------------------------

local function make_png_thumbnail(fpath)
  os.execute(fmt([[
    inkscape %s \
      --export-type=png \
      --export-background=white \
      --export-width 1200 \
      --export-height 630
  ]], fpath))
end

local function make_figure(illustrator, block, content)
  -- Assumes all outputs are of the form 'build/path/to/page/index.html'
  local page_path = path.make_relative(path.directory(PANDOC_STATE.output_file), "build")
  local hashed    = pandoc.sha1(content)
  local outname   = block.identifier ~= "" and block.identifier or hashed
  local cachefile = path.join {"cache", page_path, hashed} .. ".svg"
  local buildfile = path.join {"build", page_path, outname} .. ".svg"

  pandoc.log.info(fmt("Drawing figure: %s\n\thash: %s\n\tIllustrator: %s", buildfile, hashed, illustrator.ext))
  if not file_exists(cachefile) then
    pandoc.log.info("No cache found")
    mkparent(cachefile)
    illustrator.convert(cachefile, content)
  end
  copy(cachefile, buildfile)

  if block.attributes["png"] then
    local cachepng = path.join {"cache", page_path, hashed} .. ".png"
    local buildpng = path.join {"build", page_path, outname} .. ".png"

    pandoc.log.info(fmt("png export: %s", buildpng))
    if not file_exists(cachepng) then
      make_png_thumbnail(cachepng)
    end
    copy(cachepng, buildpng)
  end

  return format_tag(outname .. ".svg", block)
end

return {
  {
    traverse = "topdown",

    -- Some environments are configurable via raw blocks in their adequate language.
    RawBlock = function(block)
      for _, illustrator in pairs(illustrators) do
        if illustrator.snippet then
          illustrator.snippet(block)
        end
      end
    end,

    Block = function(block)
      -- Turn blocks containing only a svg into a figure with a raw <object> tag
      if block.content and #block.content == 1 and block.content[1].tag == "Image" then
        local img = block.content[1]
        local _, ext = path.split_extension(img.src)

        if ext == ".svg" then
          return format_tag(img.src, img)
        end
      end
    end,

    -- Convert matching code blocks to figures
    CodeBlock = function(block)
      for _, illustrator in pairs(illustrators) do
        if illustrator.match(block.classes[1]) then
          local text = illustrator.format(block)
          return make_figure(illustrator, block, text)
        end
      end
    end,
  }
}
