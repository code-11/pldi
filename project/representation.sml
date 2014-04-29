structure InternalRepresentation = struct

	(*things that can return a value*)
	datatype expr =
		(*string=method name, stmtlist = arguments*)
		ECall of string*(stmt list)
		(*a single variable*)
		| EVar of string
		| EParen of stmt

	(*things that can't return a value*)
	and stmt = 
			(*string=name, stmt is class def body*)
			 ClassDef of scope*string*stmt
			(*string1=return type, string2=name, tuple list is argument list (type,varname), stmt is meth def body*)
			| MethDef of scope*string*string*(string*string) list*stmt
			(*string1=type string2=varname stmt=value*)
			| Initial of scope*string*string*stmt
			(*string1=type str2=name*)
			| SmInitial of scope*string*string
			(*string=varname, stmt2=value*)
			| Assign of string*stmt
			(*string=methname, stmt list=args*)
			| Call of string*(stmt list)
			(*stmt1=ifValue*)
			| If of stmt*stmt
			(*stmt=ifValue, stmt1=thenValue, stmt2=elseValue*)
			| IfElse of stmt*stmt*stmt
			| While of stmt*stmt
			(*stmt=return value*)
			| Return of stmt
			| Block of stmt list
			| Comment of string
			(*stmt1=val1,str=operator,stmt=val2 *)
			| Infix of expr*string*stmt		(*a single variable*)
			| Var of string
			| Paren of stmt
			| ArrLit of stmt list
			| For3 of stmt*stmt*stmt

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
  		|	strSt (Initial (sc,itype,varname,value)) = $["Initial (",strSc sc,",",itype,",",varname,",",strSt value,")"]
  		| 	strSt (SmInitial (sc,itype,varname)) = $["SmInitial (",strSc sc,",",itype,",",varname,")"]
  		|	strSt (Assign (varname,v)) = $["Assign (",varname,",",strSt v,")"]
		|	strSt (Call(v,args)) = $ ["Call (",v,",", (strCallArgs args),")"]
		| 	strSt (If (v,body)) = $["If (",strSt v,",",strSt body,")"]
		|	strSt (IfElse (v,trueBl,falseBl)) = $["IfElse(",strSt v,",",strSt trueBl,",",strSt falseBl,")"]
		| 	strSt (While(v,block)) = $["While (",strSt v,",",strSt block,")"]
		|	strSt (Return(v)) = $["Return (",strSt v,")"]
		|	strSt (Block(stmts)) = $["Block [",$+ (map strSt stmts),"]"]
		|	strSt (Comment(stuff)) = $["Comment (",stuff,")"]
		|	strSt (Infix(val1,operator,val2)) = $["Infix(",strExpr val1,",",operator,",",strSt val2,")"]
		| 	strSt (Var (s)) = $["Var (",s,")"]
		| 	strSt (Paren(stmt)) = $["Paren(",strSt stmt,")"]
		| 	strSt (ArrLit entries)= $["ArrLit(",strCallArgs entries,")"]
		| 	strSt (For3(init,check,stmt))= $["For3(",strSt init,",",strSt check,",",strSt stmt,")"]
	and strExpr (ECall(v,args)) = $ ["ECall (",v,",", (strCallArgs args),")"]
		| strExpr (EVar (s)) = $["EVar (",s,")"]
		| strExpr (EParen(stmt)) = $["Paren(",strSt stmt,")"]

	and strArg (str1,str2) = $["(",str1,",",str2,")"]
	and strArgs argsList= $["[",$+ (map strArg argsList),"]"]
	and strCallArgs argsList = $["[",$+ (map strSt argsList),"]"]
	and strSc (Private)="Private"
		|strSc (Public)="Public"
		|strSc (Protected)="Protected"
		|strSc (Default)="Default"
end