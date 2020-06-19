param myConst: int = 3;
var myInt: [5][5][5][5][5][5][5]int;
var myBool: bool = false;

proc myFunction(x:int, y:real) : real {
  var newResult: real = zzzz; // error not defined
  var counter: int = 0+"I believe I can fly"; // incompatible
  myConst = 10; // cannot be modified
  myFunction(5, true); // error type parameter
  myFunction(true, true); // error type parameter
  while myInt { // error not bool 
    var myBool: bool = true;
    if counter == x then myBool = 5;
    writeReal("Ciao"); // error not real
    readInt();
    x = readReal();

  }
  for x in {counter..10} do {
    writeInt(myInt[x]); // error not int
    x += 1; // x cannot be modified 

    x *= 5; // x cannot be modified 
    x = true; // x cannot be modified 
  }
  return newResult; 
  }

  proc fib(n:int): int {
    if ( false ) {
        //error no return type here!!
    }else {
      writeInt(n);
      return 0;
    }

    
  }

proc main(): void{
  return;
}