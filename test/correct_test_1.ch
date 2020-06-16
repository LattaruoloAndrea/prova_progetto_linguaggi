// different types
var myInt: int = -1000; 
var myReal: real = 9.876; 
var myBool: bool = false;
var myChar: char = 'a';
var myStr: string = "Some string..."; 
var myInit: real;
var myRealVar: real = -1.234;

// constants 
param myConstInt: int = -1000; 
param myConstReal: real = 9.876; 
param myConstBool: bool = false;
param myConstChar: char = 'a';
param myyConstVoid: string = "No";
param myConstStr: string = "Some string..."; 
param myConstInit: real = 1.5;
param myConstRealVar: real = -1.234;

param weird: [5]int = [1,2,3,4, myConstInt*5];


proc main(): void{
	return;
}