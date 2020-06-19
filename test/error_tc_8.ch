param myConst: int = 3;
var myInt: [5]int = [1,2,3,4,5];
var myBool: bool = false;

proc nyFunction(x:int, y:real) : real {
  
  var newResult: real = 0.0;
  var counter: int = 0;

  while myBool {
    if counter == x then myBool = true;
    counter+=1;
    return myBool; // first error to be visualized
  }
  
  while true do x+=1;

  for x in {counter..10} do writeInt(myInt[x]);

  for y in {1 .. 20}
   {
    if false
    {
      var p:int = 0;
      while false
      {
        for x in {counter..10} do writeInt(myInt[x]);
      }
    }
    else {
          var x:int = 0;}
   }

  return newResult; 
  }
  
  proc fib(n:int): int {
    if ( false ) {
        //second error no return type here!!
    }else {
      writeInt(n);
      return 0;
    }  
  }


proc main(): void{
  return;
}