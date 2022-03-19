![image](https://user-images.githubusercontent.com/87673997/156028540-7a94db51-dd90-4bc6-9718-96e056d24cab.png)

Clue is a programming language that compiles into Lua code with a syntax similar to languages like C or Rust.

Clue tries to be almost as simple as Lua (with only a slightly more advanced syntax) but adds many optional features that can make code look better or make some things (like metatables) easier to code.

Clue does not compile to a specfic version of Lua: flags can be toggled to alter the output to allow most if not all versions or modifications of Lua to be compiled to with Clue.

## How to install and use
1. Download the latest release and save it somewhere
2. Open your system environment variables
3. Add the path to `clue.exe` in the PATH variable
4. Type `clue` in cmd/PowerShell and it'll explain how to do the rest

## General syntax differences
- Code blocks are now inside `{}` instead of `then`/`do`/`repeat` and `end`/`until`
- `;` cannot be omitted most of the time.
- Comments are made with `// ...` or `/* ... */`

If you want a complete documentation of every change and addition in Clue check [the wiki](https://github.com/ClueLang/Clue/wiki).

## More coming soon!
There are still some features that I'm considering adding and others that will be added soon.
The most likely ones to be added in the future are:
- `switch`
- `match`
- `struct`

For any suggestion or bug you can make a github issue.

I hope Clue will be useful to you :)

## Why is Clue named Clue?
~~I have no *clue*.~~
