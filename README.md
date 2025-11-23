# hlox

Creating a lox interpreter in Haskell!

Code is translated from the first interpreter in [this book](https://craftinginterpreters.com/) by Robert Nystrom. The code here (created with `stack build`) passes the jlox test cases.

## Usage

`stack run` starts the lox REPL. If given a series of statements, the REPL evaluates all the statements with proper lox interpreter semantics. The interpreter also supports expressions, and will show the output of the current interpreter evaulating the expression.

`stack run <filename>` evaluates the specified file as a lox source code file.

## About the Interpreter

The interpreter is split into the following four phases:

1. The *scanner phase* (defined in `src/Phases/Scanner.hs`) converts an input list of characters into a list of tokens. The scanner is built off the `Parser` monad defined in `src/Parser.hs`, inspired by [tsoding's Haskell JSON parser](https://github.com/tsoding/haskell-json).

2. The *parse phase* (defined in `src/Phases/Parse.hs`) converts an input list of tokens into a list of lox statements or a single lox expression, depending on the function called. This phase is also based on the `Parser` monad, but incorporates an extra `MaybeT` monad layer to denote when a specific parsing should fail, rather than choosing another option.

3. The *resolve phase* (defined in `src/Phases/Resolver.hs`) takes an input list of statements and creates a table for local variables, defining how many environments away a given local variable is defined. It is written in point-free style.

4. The *interpret phase* (defined in `src/Phases/Interpreter.hs`) takes an input list of statements and a local variable table and interprets the statements. The main structure is the `InterpreterOutput` monad stack with the following layers, from innermost to outermost:

    1. `IO`: to log values called by the lox `print` function.
    2. `ExceptT Error`: to throw a lox runtime error.
    3. `StateT Interpreter`: to store the interpreter state.
    4. `ExceptT SomeValue`: to act an error to catch for the lox `return` keyword.

Other pieces of the code:

- The `src/Error.hs` file contains the various type of errors that can occur in lox. It also has an `UnknownError` constructor, which is thrown when, based on the invariants in the interpreter, code cannot be reached, but I cannot figure out a way to get the compiler to believe that. If an `UnknownError` is every thrown, something is wrong with the implementation.
- The `src/Tokens.hs`, `src/Phases/Expr.hs`, and `src/Phases/Stmt.hs` files contain the token, expression, and statement types that the previous phases operate on. Notably, `Expr` and `Stmt` are GADTs, to allow for more compile-time checks of correctness.

## Future work

1. Garbage collection of environments. As of writing, the interpreter stores all environments that were ever evaluated, even if they are unreachable by the program.
