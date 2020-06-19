var myInt: [5][5][5][5][5][5][5]int;


proc main(): void{

  for x in {1..10} do {
    break; // error goes only in while loop
    continue; // error goes only in while loop
  }

  while true{
  continue;
  break;
  }

  return;
}