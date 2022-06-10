--[[
  Turns code block with id 'tikz' or 'tikzcd'
  into embedded vector images.

  Depends on:
    - pdflatex
    - dvisvgm

  Code adapted from diagram-generator.lua in
    https://github.com/pandoc/lua-filters/blob/master/diagram-generator/diagram-generator.lua
]]
local tikz_template = [[
\documentclass[tikz]{standalone}
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

local fmt = string.format

local function format_tikz(body, env, pkgs, libs, preamble)
  return tikz_template:format(pkgs, libs, preamble, env, body, env)
end

local extension_for = {
  html   = 'svg',
  html4  = 'svg',
  html5  = 'svg',
  latex  = 'pdf',
  beamer = 'pdf' }

local mimetype_for = {
  svg = 'image/svg+xml',
  pdf = 'application/pdf'
}

local cachedir = 'cache/'

local function mkdir(dirname)
 os.execute("[ ! -d " .. dirname .. " ] && mkdir " .. dirname)
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

-- decoding
local function decodeURI(data)
  data = string.gsub(data, '[^'..b..'=]', '')
  return (data:gsub('.', function(x)
    if (x == '=') then return '' end
    local r,f='',(b:find(x)-1)
    for i=6,1,-1 do r=r..(f%2^i-f%2^(i-1)>0 and '1' or '0') end
    return r;
  end):gsub('%d%d%d?%d?%d?%d?%d?%d?', function(x)
    if (#x ~= 8) then return '' end
    local c=0
    for i=1,8 do c=c+(x:sub(i,i)=='1' and 2^(8-i) or 0) end
      return string.char(c)
  end))
end

local function tex2image(src, filetype)
  return pandoc.system.with_temporary_directory('tikz2image', function (tmpdir)
    return pandoc.system.with_working_directory(tmpdir, function()
      local file_template = "%s/tikz-image.%s"
      local tikzfile = file_template:format(tmpdir, "tex")
      local outfile  = file_template:format(tmpdir, filetype)
      -- Create tex source file
      local file = io.open(tikzfile, 'w')
      file:write(src)
      file:close()
      os.execute('cp ' .. tikzfile .. ' /tmp/')
      if filetype == 'pdf' then
      -- Compile tex -> pdf
        pandoc.pipe("pdflatex", {"-output-directory", tmpdir}, tikzfile)
      elseif filetype == "svg" then
        local dvifile  = file_template:format(tmpdir, "dvi")
        -- Compile tex -> dvi -> svg
        pandoc.pipe("latex", {"-output-directory", tmpdir}, tikzfile)
        os.execute("dvisvgm --scale=2 --optimize=all --font-format=woff -o " .. outfile .. " " .. dvifile)
      end
      -- Try to open and read the image:
      local img_data
      local r = io.open(outfile, 'rb')
      if r then
        img_data = r:read("*all")
        r:close()
      end
      return img_data
    end)
  end)
end

local function cache_fetch(fname)
  local f = io.open(fname, 'rb')
  if f then
    local img = f:read("*all")
    f:close()
    return img
  end
  return nil
end

local function cache_write(img, fname)
  mkdir(cachedir)
  res, err, code = os.rename(cachedir, cachedir)
  if res then
    local f = io.open(fname, 'w')
    f:write(img)
    f:close()
    return true
  end
  print("Warning: cache directory '".. cachedir .."' not found.")
  return false
end

local function mkblock(img, filetype, attrs)
  local mimetype = mimetype_for[filetype] or 'image/svg+xml'
  if filetype == 'svg' then
    -- Embed image in html
    content = "data:image/svg+xml;base64," .. encodeURI(img)
  else
    -- Hash the figure name and content:
    content = pandoc.sha1(img) .. "." .. filetype
  end
  -- Store the data in the media bag:
  pandoc.mediabag.insert(content, mimetype, img)
  return pandoc.Para {pandoc.Image({}, content, "" )}
end

function CodeBlock(block)
  local filetype = extension_for[FORMAT] or 'svg'
  local additionalpkgs = block.attributes["usepackage"] or ''
  local tikzlibs = block.attributes["tikzlibrary"] or ''
  local preamble = block.attributes["preamble"] or ''
  for _, k in pairs({"usepackage", "tikzlibrary", "tikzset"}) do
    block.attributes[k] = nil
  end
  local latexenv = ''
  if block.classes[1]   == "tikz"   then
    latexenv = "tikzpicture"
  elseif block.classes[1] == "tikzcd" then
    latexenv = "tikzcd"
    additionalpkgs = additionalpkgs .. ",tikz-cd"
  elseif block.classes[1] == "forest" then
    latexenv = "forest"
    additionalpkgs = additionalpkgs .. ",forest"
  else -- Not a codeblock we care about
    return nil
  end
  -- Substitute data on tikz template
  local fullsrc = format_tikz(block.text, latexenv, additionalpkgs, tikzlibs, preamble)
  -- Verify cached data
  local fname = cachedir .. pandoc.sha1(fullsrc) .. "." .. filetype
  local img = cache_fetch(fname)
  if img then
    return mkblock(img, filetype, block.attributes)
  end
  -- Actually produce image
  local success, img = pcall(tex2image, fullsrc, filetype)
  if success and img then
    cache_write(img, fname)
    return mkblock(img, filetype, block.attributes)
  else
    -- Error print image content and leave
    io.stderr:write(tostring(img))
    io.stderr:write('\n')
    error 'Image conversion failed'
  end
  return nil
end
