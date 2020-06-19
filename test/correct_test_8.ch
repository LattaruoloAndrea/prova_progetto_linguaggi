var myChar: char = 'a';
var myInt: int = 1;
var myReal: real = 9.876; 
var myBool: bool = false;

// correct type checking

var x: real = myReal + myBool; 
var y: real = myReal + myChar;
var z: real = myReal + myInt;
var f: int = myBool;
var t: int = myChar;

proc main(): void{
  return;
}