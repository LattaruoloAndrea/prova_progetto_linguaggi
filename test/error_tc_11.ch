param myConst: int = 3;
var myInt: [5]int = [1,2,3,4,5];
var myBool: bool = false;

proc nyFunction(p:int,q:real) : real {
  
  var newResult: real = 0.0;
  var x:bool = false;     // this x is not seen inside the loop
  var y:bool = false;
  y = x;                  //here works
  for x in {1 .. 20}
   {
    y = x;                //error type y(bool) x(int) here is the x of the loop for 
   }
   y = x;                 //here works
   y = myBool;
  return newResult; 
  }

  proc main(): void{
  return;
}