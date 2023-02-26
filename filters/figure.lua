local system = require "pandoc.system"

local fmt = string.format

-------------------
-- Caching
-------------------

local function mkdir(dirname)
 os.execute("[ ! -d " .. dirname .. " ] && mkdir " .. dirname)
end

local cachedir = 'cache/'

local function cache_fetch(fname)
  local f = io.open(fname, 'rb')
  if f then
    local img = f:read("*all")
    f:close()
    return img
  end
end

local function cache_write(fname, img)
  mkdir(cachedir)
  res, err, code = os.rename(cachedir, cachedir)
  if res then
    local f = io.open(fname, 'w')
    if f then
      f:write(img)
      f:close()
      return true
    end
  end
  error(fmt("cache directory '%s' not found.", cachedir))
  return false
end

-------------------
-- Block types
-------------------

local tikz_template = [[
\documentclass{standalone}

\def\pgfsysdriver{pgfsys-dvisvgm.def}

\usepackage{xcolor,lmodern,amsfonts,tikz}

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

local function format_tikz(body, env, pkgs, libs, preamble)
  return tikz_template:format(pkgs, libs, preamble, env, body, env)
end

local function islatex(s)
  return s == "tikz" or s == "tikzcd" or s == "forest"
end

-- Turn a latex string into an svg string.
local function latex_to_svg(code)
  local tex2svg = system.get_working_directory() .. "/scripts/tex2svg"
  return system.with_temporary_directory("tikz2image", function(tmpdir)
    return system.with_working_directory(tmpdir, function()
      local file_template = "%s/tikz-image.%s"
      local texfname = file_template:format(tmpdir, "tex")
      local svgfname = file_template:format(tmpdir, "svg")
      local file = io.open(texfname, 'w')
      if file then
        file:write(code)
        file:close()
      else
        error "Cannot open output svg file"
      end
      os.execute(fmt("%s %s %s", tex2svg, texfname, svgfname))
      -- Try to open and read the image:
      local img_data
      file = io.open(svgfname, 'rb')
      if file then
        img_data = file:read("*all")
        file:close()
      else
        error "Cannot open output svg file"
      end
      return img_data
    end)
  end)
end

-- Turn a string in Graphviz DOT language int on svg image.
local function dot_to_svg(code)
  local svgmin = system.get_working_directory() .. "/scripts/svg-minify"
  return pandoc.pipe(svgmin, {}, pandoc.pipe("dot", {"-Tsvg"}, code))
end

local b='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/' -- You will need this for encoding/decoding
-- encoding
local function encodeURI(data)
  return ((data:gsub('.', function(x)
    local r,b='',x:byte()
    for i=8,1,-1 do r=r..(b%2^i-b%2^(i-1)>0 and '1' or '0') end
    return r;
  end)..'0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
    if (#x < 6) then return '' end
    local c=0
    for i=1,6 do c=c+(x:sub(i,i)=='1' and 2^(6-i) or 0) end
    return b:sub(c+1,c+1)
  end)..({ '', '==', '=' })[#data%3+1])
end

local function mkblock(img)
  local mimetype = 'image/svg+xml'
  local content = "data:image/svg+xml;base64," .. encodeURI(img)
  -- Store the data in the media bag:
  pandoc.mediabag.insert(content, mimetype, img)
  return pandoc.Para {pandoc.Image({}, content, "")}
end

function CodeBlock(block)
  local success, svg
  -- Check for cached data
  local cachefile = fmt("%s/%s.svg", cachedir, pandoc.sha1(block.text))
  svg = cache_fetch(cachefile)
  if svg then
    return mkblock(svg)
  end
  -- Try code blocks
  if block.classes[1] == "dot" then
    success, svg = pcall(dot_to_svg, block.text)
  elseif islatex(block.classes[1]) then
    local additionalpkgs = block.attributes["usepackage"]  or ""
    local tikzlibs       = block.attributes["tikzlibrary"] or ""
    local preamble       = block.attributes["preamble"]    or ""
    local latexenv
    if     block.classes[1] == "tikz" then
      latexenv = "tikzpicture"
    elseif block.classes[1] == "tikzcd" then
      additionalpkgs = additionalpkgs .. ",tikz-cd"
      latexenv = "tikzcd"
    elseif block.classes[1] == "forest" then
      additionalpkgs = additionalpkgs .. ",forest"
      latexenv = "forest"
    end
    local fullsrc  = format_tikz(block.text, latexenv, additionalpkgs, tikzlibs, preamble)
    success, svg = pcall(latex_to_svg, fullsrc)
  else
    return
  end
  if success then
    cache_write(cachefile, svg)
    return mkblock(svg)
  else
    io.stderr:write(tostring(svg))
    io.stderr:write('\n')
    error "Image conversion failed"
  end
end
