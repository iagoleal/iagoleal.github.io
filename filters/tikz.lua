local tikz_template = [[
\documentclass[tikz]{standalone}
\usepackage{xcolor,tikz}
%% Additional packages: usepackage
\usepackage{%s}
%% Libraries for tikz: tikzlibrary
\usetikzlibrary{%s}
\begin{document}
\nopagecolor
\begin{%s}
%s
\end{%s}
\end{document}
]]

local function format_tikz(body, env, pkgs, libs)
    return tikz_template:format(pkgs, libs, env, body, env)
end

extension_for = {
  html = 'svg',
  html4 = 'svg',
  html5 = 'svg',
  latex = 'pdf',
  beamer = 'pdf' }

mimetype_for = {
    svg = 'image/svg+xml',
    pdf = 'application/pdf'
}

local b='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/' -- You will need this for encoding/decoding
-- encoding
function encodeURI(data)
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
function decodeURI(data)
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

local function tikz2image(src, filetype)
    return pandoc.system.with_temporary_directory('tikz2image', function (tmpdir)
        return pandoc.system.with_working_directory(tmpdir, function()
            local file_template = "%s/tikz-image.%s"
            local tikzfile = file_template:format(tmpdir, "tex")
            local pdffile = file_template:format(tmpdir, "pdf")
            local outfile = file_template:format(tmpdir, filetype)
            -- Create latex
            local file = io.open(tikzfile, 'w')
            file:write(src)
            file:close()
            os.execute('cp ' .. tikzfile .. ' /tmp/')
            -- Compile latex -> pdf
            pandoc.pipe("pdflatex", {"-output-directory", tmpdir}, tikzfile)
            print(outfile)
            -- Compile pdf -> svg
            if filetype == "svg" then
                -- pandoc.pipe("dvisvgm", {'--pdf', "-o", outfile}, pdffile)
                os.execute("dvisvgm --pdf -o " .. outfile .. " " .. pdffile)
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

function CodeBlock(block)
        local filetype = extension_for[FORMAT] or 'svg'
        local mimetype = mimetype_for[filetype] or 'image/svg+xml'
        local additionalpkgs = block.attributes["usepackage"] or ""
        local tikzlibs = block.attributes["tikzlibrary"] or ""
        local latex_env = ""
        if block.classes[1] == "tikz" then
            latex_env = "tikzpicture"
        elseif block.classes[1] == "tikzcd" then
            latex_env = "tikzcd"
            additionalpkgs = additionalpkgs .. ",tikz-cd"
        else -- Not a codeblock we care about
            return nil
        end
        -- Substitute data on tikz template
        local fullsrc = format_tikz(block.text, latex_env, additionalpkgs, tikzlibs)
        local success, img = pcall(tikz2image, fullsrc, filetype)
        if success and img then

            if filetype == 'svg' then
                -- Embed image in html
                content = "data:image/svg+xml;base64," .. encodeURI(img)
            else
                -- Hash the figure name and content:
                content = pandoc.sha1(img) .. "." .. filetype
            end
            -- Store the data in the media bag:
            pandoc.mediabag.insert(content, mimetype, img)
            return pandoc.Para({pandoc.Image({}, content)})
        else
            -- Print image content and leave
            io.stderr:write(tostring(img))
            io.stderr:write('\n')
            error 'Image conversion failed'
        end
    return nil
end
