<h1 align="center">Lox programming language</h1>
<p align="center"><i>The walk-tree interpreter</i></p>

**Original implementation:** [Crafting Interpreters book.](https://craftinginterpreters.com/contents.html)

This implementation is **_not_** compatible with the original one, see the differences [here](#differences-from-the-original-implementation).

Lox is an interpreted, dynamically typed, object-oriented, programming language designed by Robert Nystrom.

This was made for learning purposes, as an exercise to better understand the Rust programming language and compilers/interpreters in general.

**Language features:** 
- Variables
- First-class functions
- Closures
- Classes
- Inheritance
- Control flow structures (if, while, for loop)
- [Builtin functions](#builtin-functions)

### Examples
```
print "Hello, world!";
```
```
class Hello {
    // Class constructor
    init(world) {
        self.world = world;
    }

    // Bound method
    say() {
        print "Hello" + this.world;
    }
    
    // Static method
    static world() {
        print "Hello static world";
    }
}

// Prints "Hello static world"
Hello.world(); 

let hello = Hello("world");

// Prints "Hello world"
hello.say(); 

```

#### Run
[Cargo](https://doc.rust-lang.org/cargo/getting-started/installation.html) is required.
```shell
cargo install --git https://github.com/lucasig11/jlox

# Run the REPL
jlox

# Run some script
jlox examples/script.jlox
```

#### Differences from the original implementation
Functionality | Original | Ours
:- | :-: | :-:
Inheritance | `<` | `extends` 
Variable declaration | `var` | `let`
Function declaration | `fun` | `fn`
Integer types | `double` | `32-bit int` and `64-bit float`
Static methods | `not implemented` | `static`
If statement | ```if (true) // some code``` | ```if true { // some code }```

#### Builtin functions

- **Clock**
    Returns the current system time as milliseconds.
```
Usage:

print clock();
```
- **Read**
    Reads a line from `stdin` and returns as a string.
```
Usage:

print "write something: ";

let prompt = read();
print "you wrote: " + prompt;
```