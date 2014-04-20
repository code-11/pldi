structure Run =struct 

	structure P = Parser
	structure T= Translator
	structure I=InternalRepresentation

	fun genJava1 ()=
		"public static void herp(double a,float b){return b;}"

	fun genJava3 ()=
		"String spam(int a){while(a){if(a){return a;}else{return b;}}}"

	fun genJava2 ()=
		"class HelloWorld{"^genJava1()^"}"

	fun genJava4 ()=
		"{true || false; int a;}"

	fun genJava5 ()=
		"/* Blah Comment Blah */"

	fun run () = 
		let val tokenList= P.lexString(genJava4()) in
			((P.printTokens tokenList);
			print (case (P.parse_stmt tokenList)
				of NONE=>"\n"
				| SOME (stmt,ts)=> (I.strSt stmt)))
		end
			(*print (T.stringify (T.translate tokenList))*)
end