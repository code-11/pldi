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
		"(true || false)"

	fun genJava5 ()=
		"/* Blah Comment Blah */"

	fun genJava6 ()=
		"int a=(1+1);"

	fun genJava7 ()=
		"int b=((1+1));"

	fun test()=
		(run genJava1;
		run genJava2;
		run genJava3;
		run genJava4;
		run genJava5;
		run genJava6;
		run genJava7)
	and run test = 
		let val tokenList= P.lexString(test()) in
			((P.printTokens tokenList);
			print (case (P.parse_stmt tokenList)
				of NONE=>"\n!"
				| SOME (stmt,ts)=> (I.strSt stmt)^"\n\n"))
			(*print (I.strSt (I.Block([I.Infix(I.Var("true"),"||",I.Var("false")),I.SmInitial(I.Default,"int","a")]))))*)
		end
			(*print (T.stringify (T.translate tokenList))*)
end