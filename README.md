# Progetto Linguaggi e Compilatori, CHAPEL--

## Features
- Chapel like syntax
- Static scope, visibility from declaration point
- Statically typed

## TODO
- [X] Grammar

- Optional syntactic features
	- [X] `do` notation for `for`, `while` and `if`
	- [ ] no type specifier needed for initialized variable (`var x = 5 // x is int`)
	- [ ] no size required on array initialization (`var A: [ ] int = [1, 5, 25] // A has size 3`)
	- [ ] multi-declaration with same type and initialization (`var x, y, z : int = 42 // all int and =42`)

- [ ] Type checker
	- [ ] Improve error messages (maybe with a family of functions)
	- [X] Set up data types in AST
	- [ ] Construct parametric AST () for Parser
	- [X] Set the Err monad suitable for error concat and warning/fatal distinction
	- [ ] Work on parametric AST Info for checkers and inferers
	- [X] Type system

- [ ] Three Address Code

- [ ] Testing
	- [ ] Write functions to simplify construction of data types (e.g. with dummy location)