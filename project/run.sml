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
		"/* Blah Comment \n Blah */"

	fun genJava6 ()=
		"int[][] a=(1+1);"

	fun testParen ()=
		"int b=((1+1));"

	fun testBlock ()=
		"{{}{}while((a==1)){a=1;}{}{}}"

	fun testCallAsStmt ()=
		"{thisIsAStmt(); thisIsAStmt(1); thisIsAStmt(1,2);}"

	fun testCallAsValue ()=
		"thisIsAVal()+1+1"

	fun testDot ()=
		"this.this.this.that;"

	fun testArrayArgMeth ()=
		"public static int[] spam(int[] a){a=1;} "

	fun testArrayLit ()=
		"{int[] c = {}; int[] d = {1}; int[] e = {1,2}; int d[]={1,2,3};}"

	fun testArrayCons ()=
		"{int[] c=new int[4]; int c[]=new int[5];}"

	fun genLine ()=
		print "\n---------------------------------------------------\n"

	fun test()=
		(genLine();
		run genJava1;
		run genJava2;
		run genJava3;
		run genJava4;
		run genJava5;
		run genJava6;
		run testParen;
		run testBlock;
		run testCallAsStmt;
		run testCallAsValue;
		run testDot;
		run testArrayArgMeth;
		run testArrayLit;
		run testArrayCons;
		genLine())
	and run test = 
		let val tokenList= P.lexString(test()) in
			((P.printTokens tokenList);
			print (case (P.parse_stmt tokenList)
				of NONE=>"\n!"
				| SOME (stmt,ts)=> (I.strSt stmt)^"\n\n"))
			(*print (I.strSt (I.Block([I.Infix(I.Var("true"),"||",I.Var("false")),I.SmInitial(I.Default,"int","a")]))))*)
		end
			(*print (T.stringify (T.translate tokenList))*)


	fun testTrans()=
		let val SOME (stmt, ts) = (P.parse_stmt(P.lexString(testArrayLit())))
		in
			print ("\n"^(T.translate 0 stmt)^"\n\n")
		end

	fun testInput(file)=
		print(TextIO.inputAll(TextIO.openIn(file)))

	fun transInput(file)=
		(*Put text content of input file through parser*)
		let val SOME (stmt, ts) = (P.parse_stmt(P.lexString(TextIO.inputAll(TextIO.openIn(file)))))
		in
			print ("\n"^(T.translate 0 stmt)^"\n\n")
		end
end