proc fibonacci(n : int) : int {
  if n <= 1 then return n;
  return fibonacci(n-1) + fibonacci(n-2);
}

proc addThree(in n:int) in:int{
  param p: int = 5;
  return n + 3+p;
}

proc doublePrint(inout thing:string) in: string {
  var p: int = 5;
  return thing + thing;
}

proc defaultsProc(in x: int,ref y: real): real {
  return x+y;
}
