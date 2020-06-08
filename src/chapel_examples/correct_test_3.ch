proc main() : void {
  var x:int =1+1, y: real = 0.45*2;
  var alpha: bool = true || false;
  return;
}

proc fib(n: int): int {
  if n <= 1 then
    return n;
  else
    return fib(n-1) + fib(n-2);
}
