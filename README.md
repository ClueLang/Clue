# The Clue programming language

![image](https://user-images.githubusercontent.com/87673997/156028540-7a94db51-dd90-4bc6-9718-96e056d24cab.png)  
[![Crates.io](https://img.shields.io/crates/v/clue?logo=rust&style=for-the-badge)](https://crates.io/crates/clue)
[![Crates.io](https://img.shields.io/crates/d/clue?logo=rust&style=for-the-badge)](https://crates.io/crates/clue)
[![GitHub](https://img.shields.io/github/v/release/ClueLang/Clue?logo=github&color=181717&display_name=release&include_prereleases&sort=semver&style=for-the-badge)](https://github.com/ClueLang/Clue)
[![AUR](https://img.shields.io/aur/version/clue?color=1793d1&logo=arch-linux&style=for-the-badge)](https://aur.archlinux.org/packages/clue/)

Clue is a programming language that compiles blazingly fast into Lua code with a syntax similar to languages like C or Rust.

Clue tries to be almost as simple as Lua (with only a slightly more advanced syntax) but adds many optional features that can make code look better or make some things (like metatables) easier to code.

Clue does not compile to a specfic version of Lua: flags can be toggled to alter the output to allow most if not all versions or modifications of Lua to be compiled to with Clue.

## General syntax differences
- Code blocks are now inside `{}` instead of `then`/`do`/`repeat` and `end`/`until`
- Comments are made with `// ...` or `/* ... */`

If you want a complete documentation of every change and addition in Clue check [the wiki](https://github.com/ClueLang/Clue/wiki).

## Example code
```rs
@ifos linux {
	@define GREETING "Hello, Linux user "
} @else_ifos macos {
	@define GREETING "Hello, MacOS user "
} @else {
	@define GREETING "Hello, Windows user "
}
  
@macro GREET(target) { $GREETING .. $target .. "!" }
  
print($GREET!("Maiori"))

local fn add(x = 0, y = 0) {
    return x + y
}

global n = 1

while n < 10 {
    n += add($, $)
    match n {
        3 => {
            continue
        }
        4 if x => {
            break
        }
        default => {
            print(n < 3 ? n : -n)
        }
    }
}
```
More examples can be found in:
- the [wiki](https://github.com/ClueLang/Clue/wiki)
- the [Examples directory](https://github.com/ClueLang/Clue/tree/main/examples)
- a [game made with LÖVE](https://github.com/Maiori44/ip-please)
- Clue's [MessagePack library](https://github.com/Maiori44/msgpack-clue)
- a [simple graph visualizer made with LÖVE](https://github.com/ClueLang/Clue-example)

## How to install

### Using Cargo
1. Paste and run this command in the console: `cargo install clue`
2. Type `clue` in the console to run the compiler, it will explain the rest

Clue supports extra features that can be toggled when installing:
* `interpreter`: adds the `--execute` flag to let Clue run the generated output using [mlua](https://github.com/khvzak/mlua).
* `rpmalloc`: uses [rpmalloc](https://github.com/EmbarkStudios/rpmalloc-rs) to improve performance, not available on all platforms.
* `lsp`: adds the hidden `--symbols` flag to let Clue's language server gather the required data to function properly.

By default Clue enables all features.

### Using Linux packages
These can be downloaded in the [latest release](https://github.com/ClueLang/Clue/releases/latest).
* .deb
```
sudo dpkg -i clue_<version>_<arch>.deb
```
* .rpm
```
sudo rpm -i clue-<version>.<arch>.rpm
```

### Using the [AUR](https://aur.archlinux.org/clue)
* With paru
```sh
paru -S clue
```
* With yay
```
yay -S clue
```
* With makepkg
```sh
git clone https://aur.archlinux.org/clue.git
cd clue
makepkg -si
```

### Manual insallation
1. Download the [latest release](https://github.com/ClueLang/Clue/releases/latest) and save it somewhere
2. Open your system environment variables
3. Add the path to the directory that contains `clue.exe` in the PATH variable
4. Type `clue` in your cmd/PowerShell to run the compiler, it will explain the rest

## More coming soon!
There are still features that I'm considering adding and others that will be added soon.
The most likely ones to be added in the future are:
- type system (coming in 4.0)
- better error messages (comming in 4.0)

For any suggestion or bug you can make a github issue.
If you need help with the language itself, you can check out the [Discord server](https://discord.gg/EQsnWpqN3C).

I hope Clue will be useful to you :)

## Why is Clue named Clue?
~~I have no *clue*.~~
