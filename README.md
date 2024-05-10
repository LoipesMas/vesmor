# Vesmor

Vesmor is a [Fantasy Video Game Console](https://en.wikipedia.org/wiki/Fantasy_video_game_console).

It is inspired by old arcade games (such as [Star Wars from 1983](https://en.wikipedia.org/wiki/Star_Wars_(1983_video_game))) and
[Vectrex](https://en.wikipedia.org/wiki/Vectrex), a home console
with a vector display.

It features a custom scripting language, called `Vesmish`, that
is functional, declarative, statically typed and interpreted.

The limitation of this console is that it can only draw
[vector graphics](https://en.wikipedia.org/wiki/Vector_graphics)
(currently only lines, maybe more to come).

More detailed information can be found in the manual included with the devkit.

# State

It's mostly working, enough to make simple-ish games.
Some language features are not implemented yet (mainly generics-related stuff and quality of life).
I am also planning to add sound to the console one day.

# Code

This repo contains two modules: `vesmish` and `vesmor`.

## `vesmish`

`vesmish` contains a Rust crate that includes the interpreter for the language.

A CLI is not implemented yet, so it can really be only used as a library.

## `vesmor`

`vesmor` contains the console itself.
It is composed of a Rust part: [nannou](https://github.com/nannou-org/nannou) app that can be compiled natively or to WASM;
and a web part: [svelte](https://svelte.dev/) website that embeds the WASM console and includes code editor.
