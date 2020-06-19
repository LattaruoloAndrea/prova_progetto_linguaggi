//examples not defined types

proc main(): void{
	return;
}

var myBool: bool = false;
var myInt: int = 5;
var myChar: char = 'a';
var myReal: real = 0.99;
var myString: string = "ciao";
var myChecked: {3}int = [1,2,3];
var mm: {2}int = myChecked; // mismatch lenghts

proc check_type(): void{

	myChar = myChar + myChar; // error type mismatch
	myBool = myBool + myBool; //  error type mismatch
	myString = myChar + myChar; // error type mismatch
	return;
}
