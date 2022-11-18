
local Object = require 'classic'

-- [[ Utilities ]]

local function match(v, ...)
  local matchers = { ... }

  for i = 1, #matchers, 2 do
    if v:is(matchers[i]) then
      return matchers[i + 1]()
    end
  end
end

local function iota(start, stop, step, fn)
  step = step or 1
  local tbl = {}

  for i = start, stop, step do
    table.insert(tbl, fn and fn(i) or i)
  end

  return tbl
end

local function map(tbl, fn)
  local newtbl = {}
  for k,v in pairs(tbl) do
    newtbl[k] = fn(v)
  end
  return newtbl
end

-- [[ Reader structures ]]

local Infile = Object:extend()

function Infile:new(f)
  self.file = f
  self.lastchar = nil
end

function Infile:getc()
  if self.lastchar then
    local c
    self.lastchar, c = nil, self.lastchar
    return c
  end

  local c = self.file:read(1)
  if c == '' then
    return nil
  end
  return c
end

function Infile:ungetc(c)
  assert(self.lastchar == nil, "Double ungetc!")
  self.lastchar = c
end

local ReadError = Object:extend()

function ReadError:new(s)
  self.message = s
end

local FObject = Object:extend()

local FString = FObject:extend()

function FString:new(s)
  self.value = s
end

function FString:__tostring()
  return '"' .. self.value .. '"'
end

local FNumeric = FObject:extend()

function FNumeric:new(n)
  self.value = n
end

function FNumeric:__tostring()
  return tostring(self.value)
end

local FSymbol = FObject:extend()

function FSymbol:new(repr)
  self.value = repr
end

function FSymbol:__tostring()
  return self.value
end

local FCons = FObject:extend()

function FCons:new(car, cdr)
  self.car, self.cdr = car, cdr
end

local FNil = FObject:extend()

function FNil:__tostring()
  return 'nil'
end

local fnil = FNil()

function FCons:__tostring()
  local v = self
  local str = '['

  while v:is(FCons) do
    str = str .. ' ' .. tostring(v.car)
    v = v.cdr

    print('"' .. str .. '"')

    if v == fnil then
      break
    end
  end

  if v ~= fnil then
    str = str .. ' ~ ' .. tostring(v)
  end

  print('"' .. str .. '"')

  if str ~= '[' then
    str = str .. ' '
  end

  str = str .. ']'
  
  return str
end


-- [[ Reader ]]

local function read_string(f)
  local str = {}

  while true do
    local c = f:getc()

    if not c then
      return ReadError('Unterminated string literal')
    end

    if c == '"' then
      break
    end

    if c == '\\' then
      c = f:getc()

      if not c then
        return ReadError('Escape sequence expected character')
      end
    end

    table.insert(str, c)
  end

  return FString(table.concat(str))
end

local function read_symbol(f)
  local str = {}
  local c = f:getc()

  while c and c:match('[%.%a%d%-%+_!@#%$%%%^&%*=/~]') do
    table.insert(str, c)
    c = f:getc()
  end

  if c then
    f:ungetc(c)
  end

  if #str > 0 then
    return FSymbol(table.concat(str))
  else
    return nil
  end
end

local function read_numeric(f)
  local sym = read_symbol(f)

  if sym then
    local n = tonumber(sym.value)

    if n then
      return FNumeric(n)
    else
      return sym
    end
  end
end

local function skip_whitespace(f)
  local c = f:getc()

  while c and c:match('%s') do
    c = f:getc()
  end

  return c
end

local read

local function read_delimited_list(f, closer)
  local ftail = fnil
  local fhead = fnil
  local c = skip_whitespace(f)

  while c ~= closer do
    if c == '~' then
      ftail.cdr = read(f)
      c = skip_whitespace(f)
      assert(c == closer, 'Expecting end of dotted list')
      break
    else
      f:ungetc(c)
    end

    local obj = read(f)

    if ftail == fnil then
      fhead = FCons(obj, fnil)
      ftail = fhead
    else
      ftail.cdr = FCons(obj, fnil)
      ftail = ftail.cdr
    end

    c = skip_whitespace(f)
  end

  return fhead
end

function read(f)
  local c = skip_whitespace(f)

  if not c then
    return nil
  end

  if c:match('[%.0123456789]') then
    f:ungetc(c)
    return read_numeric(f)
  elseif c == '"' then
    return read_string(f)
  elseif c == '[' then
    return read_delimited_list(f, ']')
  else
    f:ungetc(c)
    return read_symbol(f)
  end
end


-- [[ Compiler ]]

local Compiler = Object:extend()

local function compile_fn(c)
  local all_compiled = {}

  local name = read(c.file)

  assert(name, 'End of file while compiling fn, expecting name')
  assert(name:is(FSymbol), 'Error while compiling fn, expecting symbol name')

  local sym = c:gensym()

  c:add_name(name.value, sym)

  c:push_scope()

  local argnum = c:num_argvalues(name.value)
  local retnum = c:num_returnvalues(name.value)
  local argnames = iota(1, argnum, 1, function() return c:gensym('arg') end)

  table.insert(all_compiled, 'local ' .. sym .. ' = function(' .. table.concat(argnames, ', ') .. ')')

  for k,v in ipairs(argnames) do
    c:pushstackvar(v)
  end

  while true do
    local obj = read(c.file)

    assert(obj, 'End of file while compiling fn')

    if obj:is(FSymbol) and obj.value == '.' then
      break
    end

    local compiled = c:compile(obj)

    table.insert(all_compiled, compiled)
  end

  table.insert(all_compiled, 'return ' .. table.concat(iota(1, retnum, 1, function(i) return c:stackvar(i) end), ', '))
  table.insert(all_compiled, 'end')

  c:pop_scope()

  return table.concat(all_compiled, '\n')
end

function Compiler:new(f)
  self.file = f
  self.nextvar = 0
  self.lexicon = {{}}
  self.stackvarnames = {{}}

  self.builtins = {
    fn = compile_fn
  }
end

function Compiler:opcode_call(name)
  local numargs = self:num_argvalues(name)
  local numrets = self:num_returnvalues(name)

  local rets = iota(1, numrets, 1, function() return self:gensym('ret') end)
  local args = iota(1, numargs, 1, function(i) return self:stackvar(i) end)

  self:popstackvars(numargs)

  for k,v in ipairs(rets) do
    self:pushstackvar(v)
  end

  if name == '+' then
    return 'local ' .. table.concat(rets, ', ' ) .. ' = ' .. table.concat(map(args, function(a) return '(' .. a .. ')' end), ' + ')
  else
    return 'local ' .. table.concat(rets, ', ' ) .. ' = ' .. name .. '(' .. table.concat(args, ', ') .. ')'
  end
end

function Compiler:num_returnvalues(name)
  return 1
end

function Compiler:num_argvalues(name)
  return 2
end

function Compiler:stackvar(n)
  local names = self.stackvarnames[#self.stackvarnames]
  assert(#names + 1 - n >= 1, 'Invalid argument ' .. n .. ' to stackvar')
  assert(#names + 1 - n <= #names, 'Invalid argument ' .. n .. ' to stackvar')
  return names[#names + 1 - n]
end

function Compiler:popstackvars(n)
  local tbl = self.stackvarnames[#self.stackvarnames]
  while n > 0 do
    assert(#tbl, 'Stack underflow')
    table.remove(tbl)
    n = n - 1
  end
end

function Compiler:pushstackvar(name)
  table.insert(self.stackvarnames[#self.stackvarnames], name)
end

function Compiler:add_name(name, value)
  self.lexicon[#self.lexicon][name] = value
end

function Compiler:push_scope()
  table.insert(self.lexicon, {})
  table.insert(self.stackvarnames, {})
end

function Compiler:pop_scope()
  table.remove(self.lexicon)
  table.remove(self.stackvarnames)
end

function Compiler:gensym(t)
  self.nextvar = self.nextvar + 1
  if not t then
    return 'v' .. self.nextvar
  end
  return 'v_' .. t .. self.nextvar
end

function Compiler:compile(obj)
  if obj then
    return match(obj,
          FNumeric, function()
            local v = self:gensym('num')
            self:pushstackvar(v)
            return 'local ' .. v .. ' = ' .. obj.value
          end,
          FSymbol, function()
            if self.builtins[obj.value] then
              return self.builtins[obj.value](self)
            else
              return self:opcode_call(obj.value)
            end
          end,
          Object, function()
            assert(false, "Unrecognized object type" .. tostring(obj))
          end)
  end
end

local c = Compiler(Infile(io.input()))
print(c:compile(read(c.file)))
