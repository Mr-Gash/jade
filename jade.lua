--[[

# comments come after numsign
# multiline comments don't exist
# a semicolon ends the current statement and also resets the stack
# NOTE: newlines and tabs are automatically removed
# a semicolon also ends a comment;

c set; # sets the next variable to be stored as c then resets the stack
2 5 +; # inserts 2 to the stack, inserts 5 to the stack, then adds them (and sets if we defined a set point)

# we can get c by just typing /c/

# strings can be created and wrapped 3 ways: <>, ", and '
# however, <> should ONLY be used for function operations.

# keep in mind that this language works from smallest -> largest in terms of importance.
# for example, when defining a function: args name <operations> funk;
# args are of least importance, then the name for the operation, then the actual operations of the function, and finally "funk" used to define it.
# to define a variable: 5 cat sdef; # value, the identifying name, then the keyword to define it.

]]

-- utils

local first, last = 1, 2 -- while it says first, it's really just the one before the last
local function ToTable( str )
	local tbl = {}
	
	for i = 1, string.len( str ) do
		tbl[i] = string.sub( str, i, i )
	end
	
	return tbl
end

local function tCopy() end
tCopy = function( t, lookup_table )
	local copy = {}
	for k, v in pairs( t ) do
		copy[ k ] = type( v ) == "table" and tCopy(v) or v
	end

	return copy
end

local function tobool( val )
	if val == "false" then return false end
	if val == "true" then return true end
	return nil
end

-- now let's get into this shit

-- define globals, woo.
local global = {}
setmetatable( global, {
	__index = function( self, key )
		return _G[ key ]
	end,
})

-- we might be calling this inside a keyword, so let's re-define it later
local function ExecuteJade() end
-- same
local function kw() end

-- all keywords get the following passed: stack, info, data
-- note that a list of the stack data is stack.data

local keywords = {
	-- prepares a variable to be defined with another function or operation
	[ "set" ] = function( stack, info )
		info.set = stack( last )
	end,
	-- wipes the variable done by set
	[ "reset" ] = function()
		info.set = false
	end,
	-- wipes the stack
	[ "wipe" ] = function( stack )
		stack.data = {}
	end,
	-- defines a variable and also performs /set/ on the variable
	[ "def" ] = function( stack, info, data )
		data[ stack(last) ] = stack(first)
		info.set = stack(first)
	end,
	-- defines a variable yet does not perform /set/ on the variable
	[ "sdef" ] = function( stack, info, data )
		data[ stack(last) ] = stack(first)
	end,
	-- wipes all but the last item in the stack
	[ "stage" ] = function( stack )
		local keep = stack(last)
		stack.data = {}

		return keep
	end,
	-- swaps the first and the last parts in the stack
	[ "swap" ] = function( stack, info, data )
		local fir, sec = stack(first), stack(last)

		stack.data[ #stack.data - 1 ] = sec
		stack.data[ #stack.data ] = fir
	end,
	-- basic math!
	[ "+" ] = function( stack, info, data )
		return type(stack(first)) == "string" and stack(first) .. stack(last) or
			stack(first) + stack(last)
	end,
	[ "-" ] = function( stack, info, data )
		return stack(first) - stack(last)
	end,
	[ "*" ] = function( stack, info, data )
		return stack(first) * stack(last)
	end,
	[ "/" ] = function( stack, info, data )
		return stack(first) / stack(last)
	end,
	-- logic
	[ "==" ] = function( stack, info, data )
		return stack(first) == stack(last)
	end,
	[ "!=" ] = function( stack, info, data )
		return stack(first) ~= stack(last)
	end,
	[ "ifthen" ] = function( stack, info, data )
		for k, v in pairs( stack.data ) do
			if not v then return false end
		end

		kw( "wipe", stack, info, data )
		return true
	end,
	[ "or" ] = function( stack, info, data )
		return stack(first) or stack(last)
	end,
	-- loop functionality: key value table <operations> loop;
	[ "loop" ] = function( stack, info, data )
		local args = tCopy( stack.data )

		local operation = table.remove( args, #args )
		local array = table.remove( args, #args )
		local val = #args > 0 and table.remove( args, #args ) or nil
		local key = #args > 0 and table.remove( args, #args ) or nil

		if type(operation) ~= "table" or not operation.funk then
			error( "invalid operation given to loop (<operation> expected, got " .. type(operation) .. ")" )
			return
		end

		if type(array) ~= "table" then
			error( "bad array given to loop (table expected, got " .. type(array) .. ")" )
			return
		end

		for k, v in pairs( array ) do
			ExecuteJade( operation.cmd, { [ key or "__loopkey" ] = k, [ val or "__loopkey" ] = v }, data )
		end
	end,
	-- arrays
	[ "array:new" ] = function( stack, info, data )
		return {}
	end,
	[ "array:insert" ] = function( stack, info, data )
		if type(stack(last)) ~= "table" then 
			error( "last type not array (table expected, got " .. type(stack(last)) .. ", " .. tostring(stack(last)) .. ")" )
			return
		end

		table.insert( stack(last), stack(first) )
	end,
	[ "array:remove" ] = function( stack, info, data )
		if type(stack(last)) ~= "table" then 
			error( "last type not array (table expected, got " .. type(stack(last)) .. ")" )
			return
		end

		table.remove( stack(last), stack(first) )
	end,
	[ "array:set" ] = function( stack, info, data )
		if type(stack(last)) ~= "table" then 
			error( "last type not array (table expected, got " .. type(stack(last)) .. ", " .. tostring(stack(last)) .. ")" )
			return
		end

		local args = tCopy( stack.data )
		local tab = table.remove( args, #args )
		local key = table.remove( args, #args )
		local value = table.remove( args, #args )

		stack(last)[key] = value
	end,
	[ "array:delete" ] = function( stack, info, data )
		if type(stack(last)) ~= "table" then 
			error( "last type not array (table expected, got " .. type(stack(last)) .. ")" )
			return
		end

		stack(last)[stack(first)] = nil
	end,
	[ "array:concat" ] = function( stack, info, data )
		if type(stack(last)) ~= "table" then 
			error( "last type not array (table expected, got " .. type(stack(last)) .. ")" )
			return
		end

		table.concat( stack(last), stack(first) )
	end,
	-- calls a function
	[ "ring" ] = function( stack, info, data )
		local args = {}
		for k, v in pairs( stack.data ) do
			if k == #stack.data then
				if type(v) ~= "function" then
					error( "did not receive function to call (expected function, got " .. type(v) .. ")" )
					return
				else
					return v( table.unpack( args ) )
				end
			else
				args[ #args + 1 ] = v
			end
		end
	end,
	-- wipes before calling
	[ "ringx" ] = function( stack, info, data )
		local args = {}
		for k, v in pairs( stack.data ) do
			if k == #stack.data then
				if type(v) ~= "function" then
					error( "did not receive function to call (expected function, got " .. type(v) .. ")" )
					return
				else
					kw( "wipe", stack, info, data )
					return v( table.unpack( args ) )
				end
			else
				args[ #args + 1 ] = v
			end
		end
	end,
	-- calls a function under a condition
	[ "dial" ] = function( stack, info, data )
		local args = tCopy( stack.data )
		local func = table.remove( args, #args )
		local condition = table.remove( args, #args )

		if not condition then return end
		print( "condition is " .. type(condition) .. ":" .. tostring(condition) )

		table.insert( args, func )

		kw( "ring", {data=args}, info, data )
	end,
	-- wipes before dialing
	[ "dialx" ] = function( stack, info, data )
		local args = tCopy( stack.data )
		local func = table.remove( args, #args )
		local condition = table.remove( args, #args )

		if not condition then return end

		table.insert( args, func )

		kw( "ring", {data=args}, info, data )
	end,
	-- defines a function
	-- should be used like: arg1 arg2 arg3 arg4 <function operations> funk;
	[ "funk" ] = function( stack, info, data )
		local args = tCopy( stack.data )
		local funk = table.remove( args, #args )
		if type( funk ) ~= "table" or not funk.funk then
			funk = "'bad funk def' stage print;"
		else funk = funk.cmd end

		local name = table.remove( args, #args )
		data[ name ] = function( ... )
			local t = {}
			local fetched = {...}
			for k, v in pairs( args ) do
				t[ v ] = fetched[ k ] or nil
			end

			ExecuteJade( funk, t, data )
		end
	end,
	-- prints text with a newline
	[ "print" ] = function( stack, _, data )
		print( table.unpack({tostring(stack(first)), #stack.data > 1 and tostring(stack(last)) or nil}) )
	end,
	-- don't do this :(
	[ "crazy" ] = function( stack )
		local condom = load( stack(last) )
		if condom then
			condom()
		end
	end,
}

kw = function( name, stack, info, data )
	return keywords[ name ]( stack, info, data )
end

local quotes = {["\""] = "\"", ["'"] = "'", ["<"] = ">"}
local function ParseJade( str )
	str = string.gsub( string.gsub( str, "\n", "" ), "\t", "" )

	local Statements = {}
	local Split = ToTable( str )

	local log = {} -- stores all of our commands before our ending /;/
	local current = ""
	local inside_string = false -- toggled by a double quote
	local quote_recursion = 0 -- for quotes with differing opening/closing characters inside itself
	local inside_comment = false -- once enabled, doesn't disable until the line ends or a /;/ ennds it.

	for count, char in pairs( Split ) do
		if inside_comment then
			if char ~= ";" and char ~= "\n" then
				goto lazycontinue
			elseif #log > 0 or current ~= "" then
				if current ~= "" then log[ #log + 1 ] = { raw = not inside_string, cmd = string.lower( current ) } end
				Statements[ #Statements + 1 ] = log

				log = {}
				current = ""

				goto lazycontinue
			end
		end

		-- continue parsing for this statement if we're inside a string or the statement isn't attempting to be ended.
		if (char ~= ";" and char ~= "\n") or inside_string then
			if not inside_string and char == "#" then
				inside_comment = true
				goto lazycontinue
			else
				if inside_string then
					if char == inside_string then
						quote_recursion = quote_recursion + 1
						current = current .. char
					elseif char == quotes[ inside_string ] then
						quote_recursion = quote_recursion - 1
						if quote_recursion <= 0 then
							inside_string = false
							log[ #log + 1 ] = { raw = false, cmd = current, funk = char == ">" }
							current = ""
							goto lazycontinue
						else
							current = current .. char
						end
					else
						current = current .. char
					end
				elseif char == " " then
					if current ~= "" then
						log[ #log + 1 ] = { raw = true, cmd = string.lower( current ) }
						current = ""
					end
				elseif quotes[ char ] then
					inside_string = char
					quote_recursion = 1
				else
					current = current .. char
				end
			end
		-- reset our stuff if we've logged anything so far
		elseif #log > 0 or current ~= "" then
			if current ~= "" then log[ #log + 1 ] = { raw = true, cmd = string.lower( current ) } end
			Statements[ #Statements + 1 ] = log

			log = {}
			current = ""
		end

		::lazycontinue::
	end

	if #log > 0 or current ~= "" then
		if current ~= "" then log[ #log + 1 ] = { raw = true, cmd = string.lower( current ) } end
		Statements[ #Statements + 1 ] = log

		log = {}
		current = ""
	end

	return Statements
end

local function evaluate( cmd, data )
	local t = {}
	local last = 1
	while true do
		local start, stop = string.find( cmd, ".", last, true )
		if not start or not stop then 
			if last ~= 1 then
				t[ #t + 1 ] = string.sub( cmd, last, #cmd )
			end
			break 
		end

		t[ #t + 1 ] = string.sub( cmd, last, stop - 1 )
		last = stop + 1
	end

	if #t > 0 then
		local function recur( look )
			local last = look
			for k, v in pairs( t ) do
				if not last then return false end
				last = last[ v ] or false
			end

			return last
		end

		return recur( data ) or cmd
	else
		return data[ cmd ] or cmd
	end
end

local function funkify( cmd )
	local t = { funk = true, cmd = cmd }
	setmetatable( t, {
		__tostring = function( self )
			return self.cmd
		end,
	})

	return t
end

ExecuteJade = function( str, temp, love )
	temp = temp or {} -- used for func rings

	local data = love or {} -- where we store our variables and etc - func rings might pass us diff data to reference
	local face = {} -- used instead of temp/data/global directly.

	setmetatable( face, {
		__index = function( self, key )
			return temp[ key ] or data[ key ] or global[ key ]
		end,

		__newindex = function( self, key, val )
			rawset( data, key, val )
		end,
	})

	local stack = { data = {} } -- reset each statement iteration; our current stack
	setmetatable( stack, {
		__index = function( self, key )
			return rawget( self.data, key )
		end,

		__newindex = function( self, key, val )
			rawset( self.data, key, val )
		end,

		__call = function( self, val, insert )
			if insert then
				--if #self.data > 1 then
				--	table.remove( self.data, 1 )
				--end

				table.insert( self.data, val )
				return true
			end

			if val == first then
				return self.data[ #self.data - 1 > 0 and #self.data - 1 or 1 ]
			elseif val == last then
				return self.data[ #self.data ]
			end

			return "uh no???"
		end
	})

	local info = {
		set = false, -- used by the keyword "set" to store variables.
	}
	local var = nil -- disposable

	local Statements = ParseJade( str )
	for count1, Commands in pairs( Statements ) do
		stack.data = {} -- our current stack

		for count2, cmd in pairs( Commands ) do
			if cmd.raw and keywords[ cmd.cmd ] then
				local var = {keywords[ cmd.cmd ]( stack, info, face )}
				if #var > 0 then
					if info.set then
						face[ info.set ] = var[1]
					else
						for a, b in pairs( var ) do
							stack( b, true )
						end
					end
				end
			else
				if tobool( cmd.cmd ) ~= nil then
					var = tobool( cmd.cmd )
				else
					var = cmd.funk and funkify(cmd.cmd) or 
					not cmd.raw and cmd.cmd or tonumber( cmd.cmd ) or 
					( cmd.cmd == "true" or cmd.cmd == "false" ) or 
					evaluate( cmd.cmd, face )
				end

				stack( var, true )
			end
		end
	end
end

-- local jade = require( "jade" )
-- jade.load( "myscript.jade" )
-- or,
-- jade.execute( '"loaded my fancy jade" print;')
return { execute = ExecuteJade, parse = ParseJade, load = function( str )
	local exec = io.read( str )
	if exec and exec ~= "" then
		ExecuteJade( exec )
	end
end }
