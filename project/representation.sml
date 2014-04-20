structure InternalRepresentation = struct
	datatype stmt = 
			(*string=name, stmt is class def body*)
			 ClassDef of scope*string*stmt
			(*string1=return type, string2=name, tuple list is argument list (type,varname), stmt is meth def body*)
			| MethDef of scope*string*string*(string*string) list*stmt
			(*string1=type string2=varname str3=value*)
			| Initial of scope*string*string*string
			(*string=varname, str2=value*)
			| Assign of string*string
			| Call of string*(string list)
			(*str1=ifValue*)
			| If of string*stmt
			(*str1=ifValue*)
			| IfElse of string*stmt*stmt
			| While of string*stmt
			(*str1=return value*)
			| Return of string
			| Block of stmt list
			| Comment of string
			(*str1=val1,str2=operator,str3=val2 *)
			| Infix of string*string*string
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
  		|	strSt (Initial (sc,itype,varname,v)) = $["Initial (",strSc sc,",",itype,",",varname,",",v,")"]
  		|	strSt (Assign (varname,v)) = $["Assign (",varname,",",v,")"]
		|	strSt (Call(v,args)) = $ ["Call (",v,",", (strCallArgs args),")"]
		| 	strSt (If (v,body)) = $["If (",v,",",strSt body,")"]
		|	strSt (IfElse (v,trueBl,falseBl)) = $["IfElse(",v,",",strSt trueBl,",",strSt falseBl,")"]
		| 	strSt (While(v,block)) = $["While (",v,",",strSt block,")"]
		|	strSt (Return(v)) = $["Return (",v,")"]
		|	strSt (Block(stmts)) = $["Block [",$+ (map strSt stmts),"]"]
		|	strSt (Comment(stuff)) = $["Comment (*",stuff,"*)"]
		|	strSt (Infix(val1,operator,val2)) = $["Infix(",val1,",",operator,",",val2,")"]
	and strArg (str1,str2) = $["(",str1,",",str2,")"]
	and strArgs argsList= $["[",$+ (map strArg argsList),"]"]
	and strCallArgs argsList = $["[",$+ argsList,"]"]
	and strSc (Private)="Private"
		|strSc (Public)="Public"
		|strSc (Protected)="Proected"
		|strSc (Default)="Default"
end