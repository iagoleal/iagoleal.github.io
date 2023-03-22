local system = require "pandoc.system"
local path   = require "pandoc.path"

local fmt = string.format

-----------------------------
-- Shell-like functionality
-----------------------------

local function memoize(f)
  local memory = {}
  return function(x)
    local fx = memory[x]
    if fx then
      return fx
    else
      memory[x] = f(x)
      return memory[x]
    end
  end
end

local sha1 = memoize(pandoc.sha1)

local function mkparent(fname)
  return pandoc.system.make_directory(path.directory(fname), true)
end

local function file_exists(fname)
  local f = io.open(fname, "r")
  if f ~= nil then
    io.close(f)
    return true
  else
    return false
  end
end

local function copy(source, target)
  mkparent(target)
  os.execute(fmt("cp --update --no-target-directory %s %s", source , target))
end

local function write(fname, img)
  mkparent(fname)
  local f = io.open(fname, 'w')
  if f then
    f:write(img)
    f:close()
    return true
  else
    error(fmt("Cannot write to file '%s'.", fname))
  end
end

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

-------------------
-- Block types
-------------------
local template_tag = [[
<div class="illustration">
  <object data="%s" type="image/svg+xml">
    <img src="%s">
  </object>
<div>
]]

local function format_tag(fname)
  return template_tag:format(fname, fname)
end

local template_tikz = [[
\documentclass{standalone}

\def\pgfsysdriver{pgfsys-dvisvgm.def}

\usepackage{xcolor,lmodern,amsfonts,tikz,pgffor,ifthen}

%% Additional packages: usepackage
\usepackage{%s}

%% Libraries for tikz: tikzlibrary
\usetikzlibrary{%s}

%% Additional preamble; loaded after tikz
%s

\begin{document}
\nopagecolor
\begin{%s}
%s
\end{%s}
\end{document}
]]

local function format_tikz(block)
    local pkgs     = block.attributes["usepackage"]  or ""
    local libs     = block.attributes["tikzlibrary"] or ""
    local preamble = block.attributes["preamble"]    or ""

    local env
    if     block.classes[1] == "tikz" then
      env = "tikzpicture"
    elseif block.classes[1] == "tikzcd" then
      pkgs = pkgs .. ",tikz-cd"
      env = "tikzcd"
    end

    return template_tikz:format(pkgs, libs, preamble, env, block.text, env)
end

local function islatex(s)
  return s == "tikz" or s == "tikzcd"
end

-- Turn a latex string into an svg string.
local function latex_to_svg(target, code)
  local tex2svg = system.get_working_directory() .. "/scripts/tex2svg"
  local svg = system.with_temporary_directory("tikz2image", function(tmpdir)
    return system.with_working_directory(tmpdir, function()
      local file_template = "%s/%s.%s"
      local texfname = file_template:format(tmpdir, sha1(code), "tex")
      local svgfname = file_template:format(tmpdir, sha1(code), "svg")

      write(texfname, code)
      os.execute(fmt("%s %s %s", tex2svg, texfname, svgfname))

      -- Try to open and read the image:
      return read(svgfname)
    end)
  end)

  write(target, svg)
end

-- Turn a string in Graphviz DOT language int on svg image.
local function dot_to_svg(target, code)
  mkparent(target)
  return pandoc.pipe("dot", {"-Tsvg", "-o", target}, code)
end


local function make_figure(svg_maker, content)
  local page_path = path.make_relative(path.directory(PANDOC_STATE.output_file), "build")
  local svgname   = sha1(content) .. ".svg"
  local cachefile = path.join {"cache", page_path, svgname }
  local buildfile = path.join {"build", page_path, svgname }

  if not file_exists(cachefile) then
    svg_maker(cachefile, content)
  end

  copy(cachefile, buildfile)
  return pandoc.RawBlock("html", format_tag(svgname))
end

function CodeBlock(block)
  if block.classes[1] == "dot" then
    return make_figure(dot_to_svg, block.text)
  elseif islatex(block.classes[1]) then
    return make_figure(latex_to_svg, format_tikz(block))
  else
    return
  end
end
