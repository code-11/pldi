structure InternalRepresentation = struct

	(*things that can return a value*)
	datatype limExpr =
		(*string=method name, stmtlist = arguments*)
		ECall of string*(expr list)
		(*a single variable*)
		| Var of string
		| Paren of expr
		| Not of expr
		| Neg of expr
		| ArrLit of expr list

	and expr=
		 LExpr of limExpr
		| EInfix of limExpr*string*expr

	(*things that can't return a value*)
	and stmt = 
			(*string=name, stmt is class def body*)
			 ClassDef of scope*string*stmt
			(*string1=return type, string2=name, tuple list is argument list (type,varname), stmt is meth def body*)
			| MethDef of scope*string*string*(string*string) list*stmt
			(*string1=type string2=varname stmt=value*)
			| Initial of scope*string*string*expr
			(*string1=type str2=name*)
			| SmInitial of scope*string*string
			(*string=varname, stmt2=value*)
			| Assign of string*expr
			(*string=methname, stmt list=args*)
			| SCall of string*(expr list)
			(*stmt1=ifValue*)
			| If of expr*stmt
			(*stmt=ifValue, stmt1=thenValue, stmt2=elseValue*)
			| IfElse of expr*stmt*stmt
			| While of expr*stmt
			(*stmt=return value*)
			| Return of expr
			| Block of stmt list
			| Comment of string*stmt
			(*stmt1=val1,str=operator,stmt=val2 *)
			| SInfix of expr*string*stmt		(*a single variable*)
			| For3 of stmt*expr*stmt*stmt
			| CheatExpr of expr


	and scope = Private
			| Public
			| Protected
			| Default

	fun $ ss = String.concat ss
  	fun $+ ss = String.concatWith "," ss
  	fun strCon n f xs = $ [n," (", $+ (map f xs), ")"]
  	fun strS s = "\""^s^"\""

  	fun strSt (ClassDef (sc,name,body))= $ ["ClassDef (",strSc sc,",",name,",",strSt body,")"]
  		|	strSt (MethDef (sc, retType,name,a,body))= $["MethDef (",strSc sc,",",retType,",",name,",",strArgs a,",",strSt body,")"]
  		|	strSt (Initial (sc,itype,varname,value)) = $["Initial (",strSc sc,",",itype,",",varname,",",strExpr value,")"]
  		| 	strSt (SmInitial (sc,itype,varname)) = $["SmInitial (",strSc sc,",",itype,",",varname,")"]
  		|	strSt (Assign (varname,v)) = $["Assign (",varname,",",strExpr v,")"]
		|	strSt (SCall(v,args)) = $ ["Call (",v,",", (strCallArgs args),")"]
		| 	strSt (If (v,body)) = $["If (",strExpr v,",",strSt body,")"]
		|	strSt (IfElse (v,trueBl,falseBl)) = $["IfElse(",strExpr v,",",strSt trueBl,",",strSt falseBl,")"]
		| 	strSt (While(v,block)) = $["While (",strExpr v,",",strSt block,")"]
		|	strSt (Return(v)) = $["Return (",strExpr v,")"]
		|	strSt (Block(stmts)) = $["Block [",$+ (map strSt stmts),"]"]
		|	strSt (Comment(stuff,stmt)) = $["Comment (",stuff,")",(strSt stmt)]
		|	strSt (SInfix(val1,operator,val2)) = $["SInfix(",strExpr val1,",",operator,",",strSt val2,")"]
		| 	strSt (For3(init,check,stmt,rest))= $["For3(",strSt init,",",strExpr check,",",strSt stmt,",",strSt rest,")"]
		| 	strSt (CheatExpr(expr))= $["CHExpr("^strExpr expr^")"]

	and strLimExpr (ECall(v,args)) = $ ["ECall (",v,",", (strCallArgs args),")"]
		| strLimExpr (Var (s)) = $["Var (",s,")"]
		| strLimExpr (Paren(expr)) = $["Paren(",strExpr expr,")"]
		| strLimExpr (Not(expr))= $["Not("^strExpr expr^")"]
		| strLimExpr (Neg(expr))= $["Neg("^strExpr expr^")"]
		| strLimExpr (ArrLit entries)= $["ArrLit(",strCallArgs entries,")"]

	and strExpr (LExpr(expr))= $["L("^strLimExpr expr ^")"]
		| strExpr (EInfix(lim,oper,expr))= $["EInfix("^strLimExpr lim^","^oper^","^strExpr expr^")"]

	and strArg (str1,str2) = $["(",str1,",",str2,")"]
	and strArgs argsList= $["[",$+ (map strArg argsList),"]"]
	and strCallArgs argsList = $["[",$+ (map strExpr argsList),"]"]
	and strSc (Private)="Private"
		|strSc (Public)="Public"
		|strSc (Protected)="Protected"
		|strSc (Default)="Default"
end