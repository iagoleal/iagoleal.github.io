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
local system = require "pandoc.system"
local path   = require "pandoc.path"

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

-- Copy contents from a file to a another, creating any directory needed in the process.
local function copy(source, target)
  mkparent(target)
  os.execute(fmt("cp --update --no-target-directory %s %s", source , target))
end

-- Write contents to a file, creating any directory needed in the process.
local function write(fname, content)
  mkparent(fname)
  local f = io.open(fname, 'w')
  if f then
    f:write(content)
    f:close()
    return true
  else
    error(fmt("Cannot write to file '%s'.", fname))
  end
end

-- Read file contents.
local function read(fname)
  file = io.open(fname, 'rb')
  if file then
    local content = file:read("*all")
    file:close()
    return content
  else
    error(fmt("Cannot read from file '%s'.", fname))
  end
end

-- Turn a file path absolute.
local function make_absolute(fname)
  if path.is_absolute(fname) then
    return fname
  else
    local pwd = system.get_working_directory()
    return path.normalize(path.join {pwd, path.make_relative(fname, pwd)})
  end
end

-------------------------
-- Output Tag Template
-------------------------

local template_tag = [[
<object data="%s" type="image/svg+xml">
  Browser lacks SVG support.
</object>
]]

local function format_tag(fname, block)
  local classes = {'class=\"', "illustration", '\"'}

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

  -- Turn a string in Graphviz DOT language into a SVG image.
  convert = function(target, code)
    return pandoc.pipe("dot", {"-Tsvg", "-o", target}, code)
  end
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
  end
}

------------------------------------------
-- Actual figure making
------------------------------------------

local function make_figure(svg_maker, content, name, block)
  -- Assumes all outputs are of the form 'build/path/to/page/index.html'
  local page_path = path.make_relative(path.directory(PANDOC_STATE.output_file), "build")
  local hashed    = pandoc.sha1(content) .. ".svg"
  local outname   = name and (name .. ".svg") or hashed
  local cachefile = path.join {"cache", page_path, hashed}
  local buildfile = path.join {"build", page_path, outname}

  if not file_exists(cachefile) then
    mkparent(cachefile)
    svg_maker(cachefile, content)
  end

  copy(cachefile, buildfile)

  return format_tag(outname, block) -- pandoc.RawBlock("html", format_tag(outname))
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

    -- Convert matching code blocks to figures
    CodeBlock = function(block)
      for _, illustrator in pairs(illustrators) do
        if illustrator.match(block.classes[1]) then
          local text = illustrator.format(block)
          return make_figure(illustrator.convert, text, block.attributes.name, block)
        end
      end
    end,
  }
}
