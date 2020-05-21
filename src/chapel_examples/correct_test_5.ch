param myConst: int = 3;
var myInt: [5]int = [1,2,3,4,5];
var myBool: bool = False;

proc nyFunction(x:int, y:real) : real {
  var newResult: real = 0.0;
  var counter: int = 0;
  while myBool {
	if counter == x then myBool = True;
	readInt(myInt[counter]);
	counter+=1;
  }
  for x in {counter..10} do writeInt(myInt[x]);
  return newResult; 
  }