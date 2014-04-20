structure Run =struct 

	structure P = Parser
	structure T= Translator
	structure I=InternalRepresentation

	fun run () = 
		let val tokenList= P.lexString("1+1") in
			((P.printTokens tokenList);
			print (case (P.parse_stmt tokenList)
				of NONE=>"\n"
				| SOME (stmt,ts)=> (I.strSt stmt)))
		end
			(*print (T.stringify (T.translate tokenList))*)
end