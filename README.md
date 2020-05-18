# Progetto Linguaggi e Compilatori, CHAPEL--

## Features
- Chapel like syntax
- Static scope, visibility from declaration point
- Statically typed

## TODO
- [ ] Grammar

- Optional syntactic features
	- [ ] `do` notation for `for`, `while` and `if`
	- [ ] no type specifier needed for initialized variable (type inference on initialization)
	- [ ] no size required on array initialization (`var A: [ ] int = [1, 5, 25]`)

- [ ] Type checker
	- [ ] Set up data types in AST
	- [ ] Construct parametric AST () for Parser
	- [ ] Set the Err monad suitable for error concat and warning/fatal distinction
	- [ ] Work on parametric AST Info for checkers and inferers
	- [ ] Type system

- [ ] Three Address Code