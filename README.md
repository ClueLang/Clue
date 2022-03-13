![image](https://user-images.githubusercontent.com/87673997/156028540-7a94db51-dd90-4bc6-9718-96e056d24cab.png)

# Clue
Clue is programming language that compiles into Lua code that has a syntax similar to languages like C or Rust.

Clue tries to be almost as simple as Lua (except the syntax) but adds many optional features that can make code look better or make some things (like metatables) easier to code.

Clue does not compile to a specfic version of Lua: flags can be toggled to alter the output and thus most if not all versions of Lua can be compiled to with Clue.

Clue will always output a single file: if more .clue files are compiled at once they will be merged togheter in `main.lua`, the output of these files will be stored as functions inside the table `_modules` (which should not be edited).

If multiple files are compiled Clue will make the Lua code start from `main.clue`, as such that file must be present and it should handle loading the other files.

(Note that Clue is still in beta and lacks some important features)

## How to install and use
1. Download the latest release and save it somewhere
2. Add the path to clue.exe in the PATH environment variable
3. Type `clue` in cmd/PowerShell and it'll explain how to do the rest

## General syntax differences
- Code blocks are now inside `{}` instead of `then`/`do` and `end`
- `;` cannot be omitted most of the time.
- Comments are made with `// ...` or `/* ... */`

(the below documentation is temporary, a proper wiki is being created right now.)

## `require`
The `require` function has been altered to load the functions inside the `_modules` table, It will still act like Lua's (it caches the return value and only calls the function once), so if you know how to use Lua's `require` you know how to use Clue's too.

There are only 2 differences:
* `.` must be used for directories instead of `\` or `/` (in Lua using the slashes was not intended anyway)
* If you try to load a file that was not compiled by Clue (not inside `_modules`) it will call Lua's `require` and try to load a `.lua` file instead (this is to allow external lua files/libraries to be used inside Clue code)


## Variables
### Locals
Local variables are defined in the same way as Lua:
```
local number = 1;
local string = "hello!"; //strings can also be multi-lined without using [[]]
local bool = false;
local x, y = 2.3, 5.8; //like in Lua multiple variables can be defined at once.
```

### Globals
While globals can be defined the same way it's done in Lua, Clue adds a `global` keyword:
```
global x = 3;
```
This way it can be easier to remember which variables are globals.

### Altering variables
Altering already defined variables works the same way as Lua, but Clue adds special operators to make some common operations easier:
```
local a, b, c, d, e, f, g, h;
a = 0;
b += 1;
c -= 2;
d *= 3;
e /= 4;
f ^= 5;
g ..= "6";
h ?= 7;
```
This code would be converted to:
```lua
local a, b, c, d, e, f, g, h;
a = 0;
b = b + 1;
c = c - 2;
d = d * 3;
e = e / 4;
f = f ^ 5;
g = g .. [[6]];
h = h and 7;
```

### Static variables
Static variables work similar to global variables, but they are not inserted into \_G (the global environment)

They can be useful when the Lua version you're using is slow with globals or you just don't want to pollute \_G

Note that due to the way Lua handles locals at file scope (yes statics are just that) you may not be able to define more than around 200 statics

To avoid issues with the scope statics must first be declared and then edited (otherwise it might use variables that don't exist at file scope and error):
```
static a, b, c; //the ; can be omitted here
a = 1;
b = a + 3;
```

### Pseudo variables (`$`)
The `$` "variable" is a new feature added by Clue, it rappresents the variable we are currently altering:
```
local a = 3;
a += $;
print(a); //this will print 6
```
When altering more variables at once a number can be put after the `$` to rappresent which variable it's referring to:
```
local a, b = "world!", "Hello ";
a, b = $2, $1;
print(a..b) //will print "Hello world!"
```
The first variable is `$1` the second is `$2` and so on. `$0` is not valid and will cause an error.

## If statements
If statements work the same way as Lua except `then` and `end` are replaced by `{}`:
```
if condition {
    //code
} elseif other {
    //more code
} else {
    //even more code
}
```

## `and`, `or` and `not`
The 3 above mentioned keywords do not exist in Clue, they have been replaced by the following:

### `?`/`&&` (`and`)
`and` has been replaced by both `?` and `&&`, you can use either symbol anywhere and it will get converted to `and`.
```
if (c1 && c2) {
    local x = true ? 3;
}
//or you can do...
if (c1 ? c2) {
    local x = true && 3;
}
```
**WIP: `?` as `and` is temporary and `?` will later be reused in true ternary.**

### `:`/`||` (`or`)
`or` too has been replaced by both `:` and `||` and either will be converted to `or`.
```
if (c1 || c2) {
    local x = false : 3;
}
//or you can do...
if (c1 : c2) {
    local x = false || 3;
}
```
**WIP: `:` as `or` is temporary and `:` will later be reused in true ternary.**

### `!` (`not`)
`not` has been replaced by only `!`, and only it will be converted to `not`.
```
local x = !true;
print(x); //will print false
```

## Functions
Clue's functions work the exact same way as Lua's but with some additions:

### Local functions
These remain the exact same as Lua's:
```
local function foo() {/*code*/}
```

### Global functions
These must be defined with the `global` keyword:
```
global function foo() {/*code*/}
```

### `fn`
The `fn` keyword can be used instead of the rather lengthy `function` keyword.

`fn` is especially useful when passing functions as arguments:
```
bar(fn() {/*code*/});
```

### Methods
Methods work the same way but are defined and called with `::` instead of `:`.
To define methods/functions inside tables the `method` keyword has to be used:
```
local t = {};
method t::foo() {/*code*/}
t::foo();
```

### Default values
A function's arguments can have default values, if that argument is nil it will set to the default value.

The arguments are checked from last to first:
```
local function foo(x = 3, y = 5) {
    //code
}
```
This code will be converted to:
```lua
local function foo(x, y)
    if y == nil then y = 5 end
    if x == nil then x = 3 end
	
end
```

## Tables
Tables themselves are created the same way they are made in Lua, but with a few additions:

### Metatables
Metatables get a little easier to do with the new `meta` keyword:
```
local mytable = {
    x = 3,
    meta index = {b = 5}
};
```
This table would have a metatable with the `__index` metamethod that points to the table `{b = 5}`.

All metamethods names are the same as Lua's but without the `__`, except for a few operator related metamethods that can use the operator itself:
```
local othertable = {
    meta + = fn() {/* ... */}
};
```

### Conditional indexing
If you're not sure if you're accessing a table or not you can use `?.` and `?::` to check beforehand:
```
x = a.b?.c;
y = a?.b.c;
z = a?.b?.c;
```
This code will be converted to:
```lua
x = (a.b and a.b.c);
y = (a and a.b.c);
z = (a and a.b and a.b.c);
```
(Note that it only checks if the variable exists, not if it's actually a table/something that can be indexed)

## Loops
Some loops remain unchanged from Lua but there are some important differences and additions:

### Numerical for loops
These loops remain unchanged from Lua:
```
for i = 1, 10 {
    print(i); //will print from 1 to 10
}

for i = 10, 1, -1 {
    print(i); //will print from 10 to 1
}
```

### Function for loops
These loops are slightly changed and now use the keywords `of`, `in` and `with`:
```
for k, v of t {
    //uses the iterator pairs with the table t
}

for k, v in t {
    //uses the iterator ipairs with the table t
}

for k, v with myiter(...) {
    //uses any custom iterator function with any amount of arguments
    //it's pretty much Lua's for loop but with a different keyword
}
```

### While loops
These loops remain unchanged from Lua:
```
while condition {
    //code
}
```

### Until loops
These loops are the opposite of a while loop, looping until the condition is true
```
until condition {
    //code
}
```
(This loop is not Clue's repeat until loop, as the condition is checked at the start of the iteration in this loop)

### Loop until loops
Lua's repeat until loops work the same way but with a different keyword:
```
loop {
    //code
} until x;
```

### Loop loops
These loops with a funny name will iterate forever until something external (like `break`) ends the loop:
```
local x = 1;
loop {
    x += 1;
    if x == 5 {break} //by the way, break is a rare example where ; can be omitted
}
```

### Continue
`continue` is obviously not a loop itself but it can be used inside them, it works like `break` but instead of ending the loop it skips to the next iteration:
```
for i = 1, 10 {
    if i == 5 {continue} //like break continue can have the ; omitted
    print(i); //this will print from 1 to 10 except 5
}
```
Clue will convert continue to `goto continue;` and add `::continue::` at the end of the loop, If the Lua version you're using already has a `continue` keyword you can tell Clue to use it instead by enabling the `-continue` flag:
```lua
--without the -continue flag
for i = 1, 10, 1 do
    if i==5 then
        goto continue;
    end
    print(i)
    ::continue::
end
```
```lua
--with the -continue flag
for i = 1, 10, 1 do
    if i==5 then
        continue;
    end
    print(i)
end
```

## Bitwise operations
Clue supports bitwise operations:
- AND -> `&`
- OR -> `|`
- XOR -> `^^`
- NOT -> `~`
- Left shift -> `<<`
- Right shift -> `>>`
When compiling Clue will use LuaJIT's bit library since Lua 5.1 does not support bitwise operations, if the Lua version you're using supports bitwise operations (and uses the same symbols as Clue's) you can tell Clue to convert the operators directly with the `-nojitbit` flag.

**WIP: Clue is not able to use LuaJIT's bit library yet, using bitwise operations without the `-nojitbit` flag will cause an error.** 


## More coming soon!
There are still many things that need to be added and some features are still in the planning stage.
The most likely ones to be added in the future are:
- `switch`
- `match`
- `struct`
- `enum`

For any suggestion or bug you can make a github issue.

I hope Clue will be useful to you :)

## Why is Clue named Clue?
~~I have no *clue*.~~
