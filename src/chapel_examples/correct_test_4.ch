
param myConst: int = 3;
var myInt: [5]int = [1,2,3,4,5]; 

proc fibonacci(n : int) : int {
  param myConst: int = 3;
  var myInt: [5]int = [1,2,3,4,5]; 
  if n <= 1 then return n;
  return fibonacci(n-1) + fibonacci(n-2);
}

var myReal: [myConst]real = [9.876,9.876,9.876]; 
var myBool: [2]bool = [true,false];

proc addThree(in n:int):int{
  param p: int = 5;
  return n + 3+p;
}

proc doublePrint(in thing:int): int {
  var p: int = 5;
  param myConst: int = 3;
  var myInt: [5]int = [1,2,3,4,5]; 
  return thing + thing;
}

proc defaultsProc(in x: int,ref y: real): real {
  return x+y;
}

var myyVoid: *void;
//var myyVoidTwo: ***void = 10^2^2^2;

proc sumProc( x: int, y: real): real {
  return x+y;
}

var myInit: [1][2][3]real;
var myRealVar: *real;


proc main(): void{
  return;
}
