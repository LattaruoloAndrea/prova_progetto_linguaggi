param myConst: int = 5;
param myConst1: bool= myConst && false; // error mysmatch
var myVar: real= 0.01;


proc f(): int{
	return f();
}

proc d(): int{
	var x: int = 90; 
	g = x; // error g not defined
	return 0;
}

proc dd(): int{
	param x: int = 90;
	x = false; // x is immutable
	x= 90; // here sees that x is immutable
	return 0;
}

proc rec(): bool{

	param x: bool=false;
	param y: int=5;
	param z: real=y; // real supertype of int 

	proc rec():int{
		proc rec(): real{
			proc rec(): bool{
				proc rec():int{
					proc rec(): real{
						return z;
					}
					return y;
				}
				return x;
			}
			return z;

		} 
		return y;
	}
	return x;
}

proc myFunc2(): bool{ 

	var x: int = myFunc2();
	var y: bool = false;
	var p: int =y; // error type mishmatch

	proc myFunc2(): int{
	return x;
	}
	proc myFunc2(x: int): int{ // error function already decleared
	return x;
	}
	proc myFunc2(x: real): real{ //error function already decleared
	return x;
	}

	proc myFunc2(x: real,y:real): real{ //error function already decleared
	return x;
	}
	x = myFunc2(); // accept the function
	y = myFunc2(); // error type mishmatch 
	x = myFunc2(5); //error function takes one argument
	return y;

	myVar = 0.02;
}

proc myFunc3(): bool{
	var x: bool= myFunc2() && myFunc3() +dd() -dd() +5 *0.1; // error mishmatch types
	return x;
}


proc main(): void{
  return;
}