param myConst: int = 3;
var myInt: [5]int = [1,2,3,4,5];
var myBool: bool = false;

proc nyFunction(x:int, y:real) : real {
  
  var newResult: real = 0.0;
  var x:bool = false; 
  var y:bool = false;

  for x in {1 .. 20}
   {
     proc nyFunction(x:int, y:real) : bool {
     	retrun y; // y is seen from outside
     }
     newResult = nyFunction(0,newResult); // error function seen here is the one inside the for loop
   }

  return newResult; 
  }