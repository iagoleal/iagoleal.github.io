local M = {}

--- Split a string at a delimiter
 function M.split(s, delimiter)
    result = {}
    for m in (s..delimiter):gmatch("(.-)"..delimiter) do
        table.insert(result, m)
    end
    return result
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

function readfile(fname, mode)
  local f = io.open(fname, "r")

  if not f then
    io.stderr:write(fmt("ERROR reading file %s\n", fname))
    return nil
  end

  local content = f:read(mode)

  f:close()

  return content
end




function M.read_metadata(fname)
  -- Read the entire file content
  local content = readfile(fname, "*a")
  local doc = pandoc.read(content, "markdown")

  return doc.meta  -- We only need the metadata
end


return M
