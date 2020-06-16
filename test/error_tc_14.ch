param Const1: int = 3;
param Const2: real = Const1 + 0.5;
param Const3: int = Const4 + 2;
param Const4: real = Const3 + 0.5; //error mutual definition + type mismatch
var k: int = 7;
param Const5: int = k + 3;       // error variables are mutable
var myBool: bool = false;
var x: int;

proc f1(x:int, y:real) : real {

  param Const1: real = 5;

  proc f11(k:int) : void {
     
    k = k + 1;
    
  }
  
  proc f12(s:string) : string {
    
    s = "ciao";

  }

  while z > 100             // error z not defined
   {
     x = x - 1;
     return;              // error return inside loop
    }

  f11(Const3);            // error cannot modify a constant
  
  return f12("ciao");          // error should return a real
  }


proc main(): void{
  return;
}
