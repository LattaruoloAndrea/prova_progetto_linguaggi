
 // bool + real
var myReal: real = 9.876; 
var myBool: bool = false;
var x: real = myReal + myBool; // error mismatch type
var myBool: bool = false+2; // error inference Rexp this is not seen by the tc


proc main(): void{
	return;
}