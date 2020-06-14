param myConst: int = 3;
var myInt: [5][5][5][5][5][5][5]int;
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
    writeInt(myInt[5][5][5][5][5][5][5]);
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
