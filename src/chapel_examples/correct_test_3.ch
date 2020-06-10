proc main() : void {
  var x:int =1+1, y: real = 0.45*2;
  var alpha: bool = true || false;


  var v: [4][2] int = [[1,2], [10,20], [100,200], [1000,2000]];
  var p: *int = & v[2][1];
  v[ v[0][1] ][1] = 17;
  return;
}

proc fib(n: int): int {
  if n <= 1 then
    return n;
  else
    return fib(n-1) + fib(n-2);
}
