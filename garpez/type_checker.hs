checkProgram :: Program -> Err -> Err Env
checkProgram prog env = checkGlobal prog env

checkGlobal :: [Global] -> Env -> Err Env
checkGlobal [] env = env
checkGlobal (x:xs) env = let env = case x of 
	GlobalDecl decl -> do
		checkDecl decl env
	FunDecl fun -> do 
		cehckFun fun env
	in checkGlobal xs env

checkDecl :: Declaration -> Env -> Err Env
checkDecl decl env = case decl of
	constDecl consts -> do 
		checkInt consts env
	VarDecl typ vars -> do
		checkVars typ vars env

checkInit :: [InitItem] -> Env -> Err Env -> "const" Id1= RExp1, Id2= RExp2, Id3= RExp3, Id4= RExp4, Id5= RExp5, Id6= RExp6, Id7= RExp7
checkInit [] env = env
checkInit((id rexp):xs) env = 
	if(checkVar id Env == String)
		then
			let typ2 = infer rexp env
			in if(typ2 == typ)
				then let env' = updateVar typ id fun
				in checkInt typ xs env'
				else ("Error: defined a variable with type different from the one decleared/ 2nd constant with different type from the previous one")
			else("Error: variable/constant already defined")

checkVars :: Type ->[DeclItem] -> Env -> Err Env
checkVars typ [] env = env
checkVars typ (x:xs) env = case x of 
	DeclItemDeclId id ->checkVat typ id env
	DeclItemInitItem id rexp -> do
		check((infer rexp env) == typ)
		checkInit typ [id rexp] env
	in checkVars typ xs env'

checkVar :: Type -> Id ->Env -> Err Env
checkVar typ id env = 
	if(type (lookVar id env) == String)
		then (return?) update typ id env
		else("error: Variable already defined")
