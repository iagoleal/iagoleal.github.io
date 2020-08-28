local system = require 'pandoc.system'

local tikz_doc_template = [[
\documentclass{standalone}
\usepackage{xcolor}
\usepackage{tikz}
\begin{document}
\nopagecolor
%s
\end{document}
]]

local function tikz2image(src, filetype, outfile)
  system.with_temporary_directory('tikz2image', function (tmpdir)
    system.with_working_directory(tmpdir, function()
      local f = io.open('tikz.tex', 'w')
      f:write(tikz_doc_template:format(src))
      f:close()
      os.execute('pdflatex tikz.tex')
      if filetype == 'pdf' then
        os.rename('tikz.pdf', outfile)
      else
        os.execute('dvisvgm --pdf -o ' .. outfile .. ' tikz.pdf')
      end
    end)
  end)
end

extension_for = {
  html = 'svg',
  html4 = 'svg',
  html5 = 'svg',
  latex = 'pdf',
  beamer = 'pdf' }

local function file_exists(name)
  local f = io.open(name, 'r')
  if f ~= nil then
    io.close(f)
    return true
  else
    return false
  end
end

local function starts_with(start, str)
  return str:sub(1, #start) == start
end

function match_dirname(str)
    return str:match("(.*[/\\])")
end

function RawBlock(el)
  if starts_with('\\begin{tikzpicture}', el.text) then
    local filetype = extension_for[FORMAT] or 'svg'
    local fname = system.get_working_directory() .. '/_cache/' .. pandoc.utils.sha1(el.text) .. '.' .. filetype
    if not file_exists(fname) then
      local cachedir = system.get_working_directory() .. '/_cache'
      os.execute("[ -d " .. cachedir .. " ] || mkdir ".. cachedir)
      tikz2image(el.text, filetype, fname)
    end
    return pandoc.Para({pandoc.Image({}, fname)})
  else
   return el
  end
end
