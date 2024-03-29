## TODO
  - [X] `declare` - Define global variables
  - [X] `let` - Define scoped variables
  - [X] `do` - Block of expressions
  - [X] `lambda` - Define lambda
  - [X] Fix numbers in variable names issue
  - [X] Bool
  - [X] Loading stdlib
  - [X] Fix scope issue for lambda
  - [X] Fix repl to maintain global scope
  - [X] `def` - Define functions
  - [X] `if`
  - [X] Comments
  - [X] Symbol String -> Symbol Expression
  - [X] `quote`
  - [X] `eval`
  - [X] List ops - `car`, `cdr`, `cons`
  - [X] Inline lambda
  - [X] Allow predefined functions to be overridden
  - [X] functions for type checking
  - [X] Fix display argument order
  - [X] Syntax for eval, quote
  - [X] Remove return value printing for evalation of file
  - [X] Allow using bindings inside definition. Eg - `(let ((a 1) (b (+ a 1))) (+ a b))`
  - [X] Support for 0 params function
  - [X] Allow rest syntax for lambda
  - [X] `display`
  - [X] Negative ints
  - [X] Variable number of args
  - [X] Refactor to have scope stack instead of a flat scope
  - [X] `cond`
  - [X] Make `=` work with any number of args
  - [X] Make `and`, `or` work with any number of args
  - [X] `compose2`
  - [X] `curry`
  - [ ] `compose`
  - [ ] Fix issue with callstack/scope for function passed as argument with the same param names
  - [ ] `use` - Import module `(use "./file.sim" (my-fn other-fn))`
  - [ ] `use` - Importing stdlib `(use math (min max))`
  - [ ] Fix leaking scope issue for (function params conflict with scope variables)
  - [ ] Better errors for argument length mismatch
  - [ ] Allow builtins to be first-class atoms
  - [ ] Improve comment syntax
  - [ ] `quasiquote`, `unquote`
  - [ ] `call-with-current-continuation`
  - [ ] Improve repl
  - [ ] Curry/Partial application syntax?
  - [ ] Float
  - [ ] String as list of chars
  - [ ] `read`
  - [ ] File io
  - [ ] `def-syntax` ?
  - [ ] Ignore comments instead of making them nil
  - [ ] More error test cases
  - [ ] Generalize errors
  - [ ] Error line numbers
  - [ ] core stdlib
  - [ ] list stdlib
  - [ ] string stdlib
