proc fibonacci(n : int) : int {
  if n <= 1 then return n;
  return fibonacci(n-1) + fibonacci(n-2);
}

proc addThree(in n:int) out:int{
  param p: int = 5;
  return n + 3+p;
}

proc doublePrint(inout thing:string) out: string {
  var p: int = 5;
  return thing + thing;
}

proc defaultsProc(in x: int,ref y: real = 1.2634): real {
  return x+y;
}
