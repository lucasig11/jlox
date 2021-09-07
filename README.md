# Lox programming language
<p align="center"><i>The walk-tree interpreter</i></p>

**Original implementation:** [Crafting Interpreters book.](https://craftinginterpreters.com/contents.html)

This implementation is **_not_** compatible with the original one, see the differences [here](#differences-from-the-original-implementation).


### Requirements
- [Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html)
### Run
After cloning the repo and `cd`ing into it, issue the following commands:

```shell
cargo install --git https://github.com/lucasig11/jlox

# Run the REPL
jlox

# Run some script
jlox examples/script.jlox
```

### Differences from the original implementation
Functionality |  Original | This 
:-: | :-: | :-:
Inheritance | `<` | `extends` 
If statement |  ```if (true) // some code``` | ```if true { // some code }```
Static methods | `not implemented` | `static`