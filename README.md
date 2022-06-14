# Haskle

Haskle is a little [wordle](https://www.nytimes.com/games/wordle/index.html)-like game,
where the goal is to guess a function from its (obfuscated) type. After each try, the
type is partially revealed, and when the type is revealed, the name of the function starts
to reveal itself as well.

By default, the set of possible answer is the [haskell prelude](https://hackage.haskell.org/package/base-4.16.1.0/docs/Prelude.html),
but you can choose from other sets (for now, lens operators, and just traverse). As a matter
of fact, adding a new set is a great way to contribute to haskle!

## Contributing

Haskle is an [elm](https://elm-lang.org/) application. I’ve tried to keep it as simple as
possible, and I mostly work on it during my breaks, so code quality is… what it is.
The nice thing about haskle is that it is working purely client-side, so it is relatively
easy to hack on it locally.

### Running locally

The only dependency is `elm-0.19`, you should be able to get it with `nix-shell` (a `shell.nix`
file is present).

```
make && xdg-open public/index.html
```

### Adding a new function set

The core part of a function set is a list of signatures, represented as strings. The name
must be separated from the type with ` :: `. In order to make the game more interesting,
types like `[a]`, `(a,b)`, and `()` have to be replaced by `List a`, `Tuple a b` and `Unit`.

Additionally, you’ll have to provide a bit of extra information (like how to generate links
to documentation pages).
