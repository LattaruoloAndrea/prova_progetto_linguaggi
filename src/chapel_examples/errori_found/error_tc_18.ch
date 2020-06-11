var par: int;

proc myFunc(): **void // I am not sure if this is an error on error type
{
	var domani: int;
	domani = par;
}

proc myFunc1(): *void // I am not sure if this is an error on the return type
{
	var domani: int;
	domani = par;
}

proc myFunc5(): **int // the mismatch type here is found, correct
{
	var domani: [4][4]int;
	return domani;
}

//the functions below don't produce errors

proc myFunc2(): void
{
	var domani: int;
	domani = par;
}

proc myFunc3(): string
{
	var domani: int;
	domani = par;
	return "sdasd";
}

proc myFunc4(): *string
{
	var domani: int;
	var y: **string;
	domani = par;
	return *y;
}


proc myFunc6(): [4][4]int
{
	var domani: [4][4]int;
	return domani;
}
