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
	- [ ] multi-declaration with same type and initialization (`var x, y, z : int = 42`)

- [ ] Type checker
	- [ ] Set up data types in AST
	- [ ] Construct parametric AST () for Parser
	- [ ] Set the Err monad suitable for error concat and warning/fatal distinction
	- [ ] Work on parametric AST Info for checkers and inferers
	- [ ] Type system

- [ ] Three Address Code