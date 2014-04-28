structure Translator =  struct

	structure P = Parser
	structure I = InternalRepresentation

	(*Takes an internal rep statement
		Recursively turns it into a Python string
		Yay?*)
	fun translate level (I.ClassDef(scope,name,body)) =
			"class "^name^":\n"^(translate level body)

		| translate level (I.MethDef (scope,typ,name,args,body)) = 
			(prodInd level)^"def "^name^"("^(transArgs args)^"):\n"^(translate level body)

		| translate level (I.Initial(scope,typ,name,value)) =
			(prodInd level)^name^"="^(translate level value)

		| translate level (I.SmInitial(scope,typ,name)) =""

		| translate level (I.Assign(name,value)) =
			(prodInd level)^name^"="^(translate level value)^"\n"
		
		| translate level (I.Call(name,args))=
			(prodInd level)^name^"("^(transCallArgs args)^")"^"\n"

		| translate level (I.If(cond,body))=
			(prodInd level)^"if("^(translate 0 cond)^"):\n"^(translate level body)

		| translate level (I.IfElse(ifCond,ifBody,elseBody)) =
			(prodInd level)^"if("^(translate 0 ifCond)^"):\n"^(translate level ifBody)^"\n"^(prodInd level)^"else:\n"^(translate level elseBody)
		
		| translate level (I.While(cond,body)) =
			(prodInd level)^ "while("^(translate 0 cond)^"):\n"^(translate level body)
			(*stmt=return value*)
		
		| translate level (I.Return(st)) =
			(prodInd level)^"return "^(translate level st)
		(*ignoring arbitrary blocks because bad style*)
		| translate level (I.Block(ss)) = transList (level+1) ss

		| translate level (I.Comment(s)) = 
			"\"\"\""^s^"\"\"\""

		(*translate I.Infix = *)		(*a single variable*)
		| translate level (I.Var s) = s
		
		(*translate I.Paren = *)
		(*translate I.ArrLit = *) 

	and transArgs [] = ""
		| transArgs [(typ,arg)] = arg
		| transArgs ((typ,arg)::rest) = (arg)^","^(transArgs rest)

	and transList level []=""
		| transList level (s::ss)=(translate level s)^(transList level ss)

	and transCallArgs []=""
		| transCallArgs [arg] = translate 0 arg
		| transCallArgs (arg::args)= (translate 0 arg)^","^(transCallArgs args)


	and prodInd level=
		case level 
			of 0=>""
			| x=>"\t"^(prodInd (x-1))

end