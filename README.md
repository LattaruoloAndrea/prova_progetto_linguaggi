# Progetto Linguaggi e Compilatori, CHAPEL--

## Features
- Chapel like syntax
- Static scope, visibility from declaration point
- Statically typed

## TODO
- [X] Grammar

- Optional syntactic features
	- [X] `do` notation for `for`, `while` and `if`
	- [X] no type specifier needed for initialized variable (`var x = 5 // x is int`)
	- [X] no size required on array initialization (`var A: [ ] int = [1, 5, 25] // A has size 3`)
	- [X] multi-declaration with same type and initialization (`var x, y, z : int = 42 // all int and =42`)

- [X] Type checker
	- [X] Improve error messages (maybe with a family of functions)
	- [X] Set up data types in AST
	- [X] Construct parametric AST () for Parser
	- [X] Set the Err monad suitable for error concat and warning/fatal distinction
	- [X] Work on parametric AST Info for checkers and inferers
	- [X] Type system

- [X] Three Address Code