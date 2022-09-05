![image](https://user-images.githubusercontent.com/87673997/156028540-7a94db51-dd90-4bc6-9718-96e056d24cab.png)

Clue is a programming language that compiles into Lua code with a syntax similar to languages like C or Rust.

Clue tries to be almost as simple as Lua (with only a slightly more advanced syntax) but adds many optional features that can make code look better or make some things (like metatables) easier to code.

Clue does not compile to a specfic version of Lua: flags can be toggled to alter the output to allow most if not all versions or modifications of Lua to be compiled to with Clue.

## General syntax differences
- Code blocks are now inside `{}` instead of `then`/`do`/`repeat` and `end`/`until`
- Comments are made with `// ...` or `/* ... */`

If you want a complete documentation of every change and addition in Clue check [the wiki](https://github.com/ClueLang/Clue/wiki).

## Example code
```
print("Hello world!")

local fn add(x, y) {
    return x + y
}

global n = 1

while n < 10 {
    n += add(n, n)
    if n == 3 {continue}
    print(n)
}
```
More examples can be found in [the wiki](https://github.com/ClueLang/Clue/wiki), the [Examples directory](https://github.com/ClueLang/Clue/tree/main/examples) or you can check [an example program made with LOVE using Clue](https://github.com/ClueLang/Clue-example).

## How to install

### Using Cargo
1. Paste and run this command in the console: `cargo install clue`
2. Type `clue` in the console to run the compiler, it will explain the rest

### Manual insallation
1. Download the latest release and save it somewhere
2. Open your system environment variables
3. Add the path to the directory that contains `clue.exe` in the PATH variable
4. Type `clue` in your cmd/PowerShell to run the compiler, it will explain the rest

## More coming soon!
There are still some features that I'm considering adding and others that will be added soon.
The most likely ones to be added in the future are:
- more advanced macros
- types (coming in 3.0)

For any suggestion or bug you can make a github issue.
If you need help with the language itself, you can check out the new [Discord server](https://discord.gg/EQsnWpqN3C).

I hope Clue will be useful to you :)

## Why is Clue named Clue?
~~I have no *clue*.~~
