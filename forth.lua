
local Object = require 'classic'

-- [[ Utilities ]]

local Bimap = Object:extend()

function Bimap:new()
  self.key_to_value = {}
  self.value_to_keys = {}
end

function Bimap:add(k, v)
  self:remove(k)
  self.key_to_value[k] = v

  local keys = self.value_to_keys[v]
  if not keys then
    keys = {}
    self.value_to_keys[v] = keys
  end

  keys[k] = true
end

function Bimap:remove(k)
  local value = self.key_to_value[k]

  if not value then
    return
  end

  self.key_to_value[k] = nil
  self.value_to_keys[value][k] = nil
end

function Bimap:value_at(k)
  return self.key_to_value[k]
end

local function array_copy(tbl)
  local tbl = {}
  for k, v in ipairs(tbl) do
    tbl[k] = v
  end
  return tbl
end

function Bimap:keys_at(v)
  return array_copy(self.value_to_keys[v])
end

local function tabletostring(o)
  if type(o) == 'table' then
    if getmetatable(o) and getmetatable(o).__tostring then
      return tostring(o)
    end
     local s = '{ '
     for k,v in pairs(o) do
        if type(k) ~= 'number' then k = '"'..tostring(k)..'"' end
        s = s .. '['..k..'] = ' .. tabletostring(v) .. ','
     end
     return s .. '} '
  else
     return tostring(o)
  end
end

local function printtable(o)
  print(tabletostring(o))
end

local function mtch(v, ...)
  local matchers = { ... }

  for i = 1, #matchers, 2 do
    if v:is(matchers[i]) then
      return matchers[i + 1]()
    end
  end
end

local match = mtch

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

local function filter(tbl, fn)
  local newtbl = {}
  local i = 1
  for _,v in ipairs(tbl) do
    if fn(v) then
      newtbl[i] = v
      i = i + 1
    end
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
  return 'FS."' .. self.value .. '"'
end

local FNumeric = FObject:extend()

function FNumeric:new(n)
  self.value = n
end

function FNumeric:__tostring()
  return 'FN.' .. tostring(self.value)
end

local FSymbol = FObject:extend()

function FSymbol:new(repr)
  assert(repr and type(repr) == 'string', 'Expected string, got ' .. type(repr))
  self.value = repr
end

function FSymbol:__tostring()
  return 'FS.' .. self.value
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

local FunctionDefinition

local Compiler = Object:extend()

function Compiler:compile_fn()
  local all_compiled = {}

  local fname = read(self.file)

  assert(fname, 'End of file while compiling fn, expecting name')
  assert(fname:is(FSymbol), 'Error while compiling fn, expecting symbol name')

  self:push_scope()

  local argnum = self:num_argvalues(fname.value)
  local retnum = self:num_returnvalues(fname.value)
  local argnames = iota(1, argnum, 1, function() return self:gensym('arg') end)

  table.insert(all_compiled, 'local ' .. fname.value .. ' = function(' .. table.concat(argnames, ', ') .. ')')

  for _,v in ipairs(argnames) do
    self:pushstackvar(v)
  end

  local objects = {}

  while true do
    local obj = read(self.file)

    assert(obj, 'End of file while compiling fn')

    if obj:is(FSymbol) and obj.value == '.' then
      break
    end

    table.insert(objects, obj)
  end

  print('type checking')
  local constraints, intermediate_type_vars = self.typechecker:typecheck(FunctionDefinition(fname, objects))

  printtable(constraints)
  printtable(intermediate_type_vars)
  printtable(self.typechecker.var_name_to_type_var)

  print('unifying')
  self.typechecker:unify(constraints)

  for _,obj in ipairs(objects) do
    local compiled = self:compile(obj)
    table.insert(all_compiled, compiled)
  end

  table.insert(all_compiled, 'return ' .. table.concat(iota(1, retnum, 1, function(i) return self:stackvar(i) end), ', '))
  table.insert(all_compiled, 'end')

  self:pop_scope()

  return table.concat(all_compiled, '\n')
end

local TypeChecker

function Compiler:new(f)
  self.file = f
  self.nextvar = 0
  self.lexicon = {{}}
  self.stackvarnames = {{}}
  self.typechecker = TypeChecker()

  self.builtins = {
    fn = function() self:compile_fn() end
  }
end

function Compiler:opcode_call(name)
  local numargs = self:num_argvalues(name)
  local numrets = self:num_returnvalues(name)

  local rets = iota(1, numrets, 1, function() return self:gensym('ret') end)
  local args = iota(1, numargs, 1, function(i) return self:stackvar(i) end)

  self:popstackvars(numargs)

  for _,v in ipairs(rets) do
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
  assert(#names + 1 - n >= 1, 'Invalid argument (underflow) ' .. n .. ' to stackvar')
  assert(#names + 1 - n <= #names, 'Invalid argument (overflow) ' .. n .. ' to stackvar')
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

function Compiler:lookup_name(name)
  for i = #self.lexicon, 1, -1 do
    local val = self.lexicon[i][name]
    if val then
      return val
    end
  end
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
  assert(obj, "Compiler:compile expected object, got nil")

  return mtch(obj,
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
                assert(false, "Unrecognized object type " .. tostring(obj))
              end)
end

local ASTNode = Object:extend()

FunctionDefinition = ASTNode:extend()

function FunctionDefinition:new(fname --[[FSymbol]], body --[[table]])
  assert(fname and fname:is(FSymbol))
  assert(body and type(body) == 'table')
  self.fname, self.body = fname, body
end

local TypeAST = Object:extend()

local TypeFunction = TypeAST:extend()

function TypeFunction:new(args, rets)
  assert(args and type(rets) == 'number')
  assert(rets and type(rets) == 'number')

  self.args, self.rets = args, rets
end

function TypeFunction:__tostring()
  return 'TFn[' .. self.args .. ' -> ' .. self.rets .. ']'
end

local TypeCons = TypeAST:extend()

function TypeCons:new(car, cdr)
  assert(car and type(car) == 'number')
  assert(cdr and type(cdr) == 'number')

  self.car, self.cdr = car, cdr
end

function TypeCons:__tostring()
  return '(' .. self.car .. ' . ' .. self.cdr .. ')'
end

local TypeAtom = TypeAST:extend()

function TypeAtom:new(name)
  assert(name and type(name) == 'string')
  self.name = name
end

function TypeAtom:__tostring()
  return 'Atom[' .. self.name .. ']'
end

local Constraint = Object:extend()

local ConstraintsEqual = Constraint:extend()

function ConstraintsEqual:new(lhs, rhs)
  assert(lhs and type(lhs) == "number")
  assert(rhs and type(rhs) == "number")
  self.lhs, self.rhs = lhs, rhs
end

function ConstraintsEqual:__tostring()
  return self.lhs .. ' =T= ' .. self.rhs
end

local ConstraintSubset = Constraint:extend()

function ConstraintSubset:new(element_type, set_type)
  assert(element_type and type(element_type) == 'number')
  assert(set_type and type(set_type) == 'number')

  self.element, self.set = element_type, set_type
end

function ConstraintSubset:__tostring()
  return self.element .. ' ∈ ' .. self.set
end

local ConstraintIs = Constraint:extend()

function ConstraintIs:new(variable, t)
  assert(variable and type(variable) == 'number')
  assert(t and t:is(TypeAST))

  self.variable, self.t = variable, t
end

function ConstraintIs:__tostring()
  return self.variable .. ' =I= ' .. tostring(self.t)
end

TypeChecker = Object:extend()

function TypeChecker:new()
  self.next_scope = 1
  self.scopes = {0}

  self.next_type_var = 0
  self.var_name_to_type_var = {}

  self.atom_cache = {}
end

function TypeChecker:get_name_var(name)
  return self.var_name_to_type_var[name]
end

function TypeChecker:get_or_create_name_var(name)
  assert(name and type(name) == 'string', 'Expected string, got ' .. type(name))

  local var = self:get_name_var(name)

  if not var then
    var = self:getnexttypevar()
    self.var_name_to_type_var[name] = var
  end

  return var
end

function TypeChecker:get_or_create_atom(name)
  if self.atom_cache[name] then
    return self.atom_cache[name]
  end
  self.atom_cache[name] = TypeAtom(name)
  return self.atom_cache[name]
end

function TypeChecker:push_scope()
  self.next_type_var = self.next_type_var + 1
  table.insert(self.scopes, self.next_type_var)
end

function TypeChecker:pop_scope()
  table.remove(self.scopes)
end

function TypeChecker:getnexttypevar()
  self.next_type_var = self.next_type_var + 1
  return self.next_type_var + (self.scopes[#self.scopes] * 0x100000000)
end

function TypeChecker:check_function_definition(ast, constraints, intermediate_type_vars)
  assert(ast and ast:is(FunctionDefinition))

  local defins = self:getnexttypevar()

  local firstins, lastouts = nil, defins

  for _,v in ipairs(ast.body) do
    local fntype, ins, outs = self:getnexttypevar(), self:getnexttypevar(), self:getnexttypevar()

    firstins = firstins or ins
    table.insert(constraints, ConstraintIs(fntype, TypeFunction(ins, outs)))
    table.insert(constraints, ConstraintsEqual(ins, lastouts))

    match(v,
          FSymbol, function()
            local fnnamedtype = self:get_or_create_name_var(v.value)
            table.insert(constraints, ConstraintSubset(fntype, fnnamedtype))
          end,
          FNumeric, function()
            local numeric_atom = self:get_or_create_atom('number')
            local numeric_var, rest = self:getnexttypevar(), self:getnexttypevar()
            local numeric_rettype = TypeCons(numeric_var, rest)

            table.insert(constraints, ConstraintIs(numeric_var, numeric_atom))
            table.insert(constraints, ConstraintIs(outs, numeric_rettype))
            table.insert(constraints, ConstraintsEqual(rest, ins))
          end,
          FObject, function()
            assert(false)
          end)
    lastouts = outs

    intermediate_type_vars[v] = fntype
  end

  local fnid = self:get_or_create_name_var(ast.fname.value)

  table.insert(constraints, ConstraintIs(fnid, TypeFunction(firstins, lastouts)))
end

function TypeChecker:typecheck(ast --[[ASTNode]])
  assert(ast and ast:is(ASTNode))

  local constraints, intermediate_type_vars = {}, {}

  match(ast,
        FunctionDefinition, function()
          self:check_function_definition(ast, constraints, intermediate_type_vars)
        end,
        Object, function()
          assert(false)
        end)

  return constraints, intermediate_type_vars
end

function TypeChecker:unify(constraints)
  local renames = Bimap()

  local function resolvevar(x)
    return renames:value_at(x) or x
  end

  for _,constraint in ipairs(constraints) do
    match(constraint,
          ConstraintsEqual, function()
            local lhs, rhs = resolvevar(constraint.lhs), resolvevar(constraint.rhs)

            if lhs ~= rhs then
              for _,oldrename in ipairs(renames:keys_at(lhs)) do
                renames:add(oldrename, rhs)
              end
              renames:add(lhs, rhs)
            end
          end,
          Constraint, function()
            -- Do nothing with other constraints
          end)
  end

  constraints = filter(constraints, function(v) return not v:is(ConstraintsEqual) end)

  for _,constraint in ipairs(constraints) do
    match(constraint,
          ConstraintSubset, function()
            constraint.set = renames:value_at(constraint.set) or constraint.set
            constraint.element = renames:value_at(constraint.element) or constraint.element
          end,
          ConstraintIs, function()
            constraint.variable = renames:value_at(constraint.variable) or constraint.variable

            match(constraint.t,
                  TypeFunction, function()
                    constraint.t.args = renames:value_at(constraint.t.args) or constraint.t.args
                    constraint.t.rets = renames:value_at(constraint.t.rets) or constraint.t.rets
                  end,
                  TypeCons, function()
                    constraint.t.car = renames:value_at(constraint.t.car) or constraint.t.car
                    constraint.t.cdr = renames:value_at(constraint.t.cdr) or constraint.t.cdr
                  end)
          end,
          Object, function()
            assert('unexpected constraint type')
          end)
  end

  printtable(renames.key_to_value)
  printtable(constraints)
end

local c = Compiler(Infile(io.input()))
print(c:compile(read(c.file)))
