# Clue
Clue is programming language that compiles into Lua code that has a syntax similar to languages like C or Rust.

Clue tries to have a mostly open syntax and does not force any coding style.

Clue also adds many optional features to help make code smaller and implementing hard things easier and/or with a better syntax


(Note that Clue is still in beta and lacks some important (and not important) features)

## How to install and use
1. Download the latest release and save it somewhere
2. Add the path to clue.exe in the PATH environment variable
3. Type `clue` in cmd/PowerShell and it'll explain how to do the rest

## General syntax differences
- Code blocks are now inside `{}` instead of `then`/`do` and `end`
- `;` cannot be omitted most of the time.
- Comments are made with `// ...` or `/* ... */`

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
This block of code would be converted to:
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
**WIP: `elsif` and `else` are not yet supported**

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
**WIP: `?` as `and` is temporary and `?` will later be reused in true ternary**

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
**WIP: `:` as `or` is temporary and `:` will later be reused in true ternary**

### `!` (`not`)
`not` has been replaced by only `!`, and only it will be converted to `not`.
```
local x = !true;
print(x); //will print false
```

## Functions (WIP)
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
The `fn` keyword can be used instead of the rather lengthy `function` keyword
`fn` is especially useful when passing functions as arguments:
```
bar(fn() {/*code*/});
```

### Methods
Methods work the same way but are defined and called with `::` instead of `:`:
```
local t = {};
global fn t::method() {/*code*/}
t::method();
```
**WIP: Methods must currently be defined with `global function`, this may change (a `method` keyword maybe?)**

### Default values
A function's arguments can have default values, if that argument is nil it will set to the default value.
The arguments are checked from last to first:
```
local function foo(x = 3, y = 5) {
    //code
}
```
will be converted to:
```lua
local function foo(x, y)
	if y == nil then y = 5 end
	if x == nil then x = 3 end
	
end
```

### `return`
**WIP: `return` currently has to return an expression, use `return nil;` instead of `return;`**

## Tables
Tables themselves are created the same way they are made in Lua, but metatables get a little easier to do with the new `meta` keyword:
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

## PGet (`?>`)
PGet (Protected Get) works similar to `pcall` but it's used instead to check if an expression would cause an error by returning nil if it does:
```
local t = {};
local e = ?>(t.b.c); //without PGet this would cause an error 
print(e); //but thanks to PGet this will instead just print nil
```
Another case where PGet can be used is when you're not sure if a variable is a function:
```
local foo = 1;
local bar = ?>(foo()) //foo is not a function so bar becomes nil
```
**WIP: PGet is parsed but not compiled, using it will result in a crash**

## Loops (WIP)
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

### Loop until loops
Lua's repeat until loops but with a slightly different syntax:
```
loop until x {
  //code
}
```
If you instead prefer a syntax closer to Lua's this is valid too:
```
loop {
  //code
} until x;
```
**WIP: The first way of writing loop until loops may be reused for another type of loops in the future**

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
