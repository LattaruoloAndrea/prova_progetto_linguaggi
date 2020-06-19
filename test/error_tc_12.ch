param myConst: int = 3;
var myInt: [5]int = [1,2,3,4,5];
var myBool: bool = false;

proc nyFunction(x:int, y:real) : real {
  
  var newResult: real = 0.0;
  var x:bool = false; //error double declaration
  var y:bool = false; //error double declaration

  for x in {1 .. 20}
   {
     proc nyFunction(x:int,p:real) : bool {
     	return y; // y is seen from outside
     }
     newResult = nyFunction(0,newResult); // error: the function seen here is the one inside the for loop
   }

  return newResult; 
  }
  
proc main(): void{
  return;
}