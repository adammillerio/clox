# clox

An annotated C implementation of the lox language from crafting interpreters.

Lox was created by Robert Nystrom as part of the [Crafting Interpreters](https://craftinginterpreters.com/)
book.

The sample repo has a comprehensive test suite which `clox` passes as well. Refer
to the testing section of the README for more info. Function and class documentation
is also provided, mostly just as the notes I took while working through the book.

# Usage

There is a `clox.sh` script which can be used to invoke `clox` from anywhere. A C
compiler/linker and `make` are also required.

The CLI functions exactly like the reference `clox` implementation found in the
author's [example code repo](https://github.com/munificent/craftinginterpreters).

# Testing

[Original Docs](https://github.com/munificent/craftinginterpreters?tab=readme-ov-file#testing)

The sample repo includes a test suite of lox files for validation. It uses the
dart language, which can be installed via Homebrew on macOS:

```bash
brew install dart-lang/dart/dart@2.19
# brew link will make this the "primary" version of dart on your machine
# Alternatively, it can be added to the PATH for the current session:
# export PATH="$(brew --prefix)/opt/dart@2.19/bin:$PATH"
brew link dart-lang/dart/dart@2.19
```

The `build.sh` script will clone a copy of the sample repo, build `clox` and the
dart test snapshot, then run the full `clox` test suite:

```bash
./build.sh test
All 246 tests passed (568 expectations).
```

## Formatting

Code in this repo is formatted with `clang-tidy`.
