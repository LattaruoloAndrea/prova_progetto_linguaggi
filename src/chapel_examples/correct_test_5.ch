param myConst: int = 3;
var myInt: [2][2][1][1][1][2][2]int;
var myBool: bool = false;

proc myFunction(x:int, y:real) : real {
  var newResult: real = 0.00;
  var counter: int = 0;
  while true {
    var myBool: bool = true;
    if counter == x then myBool = true;
    writeReal(newResult);

  }
  for x in {counter..10} do {
    writeInt(myInt[x / x][1][x / 10][0][0][1][1]);
  }
  return newResult; 
  }

  proc fib(n:int): int {
    if ( false ) {
      return n+fib(n-1);
    }else {
      writeInt(n);
      return 0;
    }

    
  }


proc main(): void{
  return;
}