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
			(prodInd level)^name^"="^(transExpr value)^"\n"

		| translate level (I.SmInitial(scope,typ,name)) =""

		| translate level (I.Assign(name,value)) =
			(prodInd level)^name^"="^(transExpr value)^"\n"
		
		| translate level (I.SCall(name,args))=
			(prodInd level)^name^"("^(transCallArgs args)^")"^"\n"

		| translate level (I.If(cond,body))=
			(prodInd level)^"if("^(transExpr cond)^"):\n"^(translate level body)

		| translate level (I.IfElse(ifCond,ifBody,elseBody)) =
			(prodInd level)^"if("^(transExpr ifCond)^"):\n"^(translate level ifBody)^"\n"^(prodInd level)^"else:\n"^(translate level elseBody)
		
		| translate level (I.While(cond,body)) =
			(prodInd level)^ "while("^(transExpr cond)^"):\n"^(translate level body)
			(*stmt=return value*)
		
		| translate level (I.Return(st)) =
			(prodInd level)^"return "^(transExpr st)
		(*ignoring arbitrary blocks because bad style*)
		| translate level (I.Block(ss)) = transList (level+1) ss

		| translate level (I.Comment(s,stmt)) = 
			"\"\"\""^s^"\"\"\"\n"^(translate level stmt)

		| translate level (I.SInfix(val1,oper,val2))=
			let fun trans val1 oper val2= (transExpr val1)^" "^oper^" "^(translate 0 val2) in
			(case oper
				of "&&"=> let val oper="and" in (trans val1 oper val2) end
				|  "||"=> let val oper="or" in (trans val1 oper val2) end
				|   _  => (trans val1 oper val2))
			end
		

		| translate level (I.For3(init,check,stmt,rest))=
			(prodInd level)^(translate 0 init)^(prodInd level)^"while("^(transExpr check)^"):\n"^(translate level rest)^(prodInd (level+1))^(translate level stmt)

		| translate level (I.CheatExpr(expr))=
			(transExpr expr)

	and transExpr (I.EInfix(val1,oper,val2))=
			(transLimExpr val1)^" "^oper^" "^(transExpr val2)

		| transExpr (I.LExpr (lexpr))=
			(transLimExpr lexpr)

	and transLimExpr (I.ECall(name,args))= 
			name^"("^(transCallArgs args)^")"	

		| transLimExpr (I.Var(name))=
			name

		| transLimExpr (I.Paren(stmt))=
			"("^(transExpr stmt)^")"

		| transLimExpr (I.Not(stmt))=
			" not "^(transExpr stmt)

		| transLimExpr (I.Neg(stmt))=
			" -"^(transExpr stmt)	

		| transLimExpr (I.ArrLit(entries)) =
			"["^transCallArgs entries^"]"  
	

	and transArgs [] = ""
		| transArgs [(typ,arg)] = arg
		| transArgs ((typ,arg)::rest) = (arg)^","^(transArgs rest)

	and transList level []=""
		| transList level (s::ss)=(translate level s)^(transList level ss)

	and transCallArgs []=""
		| transCallArgs [arg] = transExpr arg
		| transCallArgs (arg::args)= (transExpr arg)^","^(transCallArgs args)


	and prodInd level=
		case level 
			of 0=>""
			| x=>"\t"^(prodInd (x-1))

end