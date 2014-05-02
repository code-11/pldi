structure Run =struct 

	structure P = Parser
	structure T= Translator
	structure I=InternalRepresentation

	fun genJava1 ()=
		"public static void herp(double a,float b){return b;}"

	fun genJava3 ()=
		"String spam(int a){while(a&&b){if(a){return a;}else{return b;}}}"

	fun genJava2 ()=
		"class HelloWorld{"^genJava1()^"}"

	fun genJava4 ()=
		"if(true || false){}"

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
		"if(this.this.this.that){}"

	fun testArrayArgMeth ()=
		"public static int[] spam(int[] a){a=1;} "

	fun testArrayLit ()=
		"{int[] c = {}; int[] d = {1}; int[] e = {1,2}; int d[]={1,2,3};}"

	fun testArrayCons ()=
		"{int[] c=new int[4]; int c[]=new int[5];}"

	fun testFor3 ()=
		"for(int i=1;i<6; i+=1){spam();}"

	fun testDPlus () =
		"{j++; j--;}"

	fun testInfix ()=
		"i+=1;"(* i=1+1; i=j!=k;}"*)

	fun testUnary ()=
		"{if(-1){a=1;}if(!k){a=!b;}}"

	fun testNest ()=
		"return node.getValue();"
		(*"if(node.getValue()){i++;}"
*)
	fun testIfs ()=
		"public static void main(){"^testUnary()^"}"

	fun nestInfix ()=
		"if (n==null && n==null){c=3;}"	

	fun testArrayMath() =
		"{int b = a[1+1];}"

	fun testCommentStuff() =
		"/*blah*/ int a = 5;"

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
		run testFor3;
		run testDPlus;
		run testInfix;
		run testUnary;
		run testNest;
		run testIfs;
		run nestInfix;
		run testArrayMath;
		run testCommentStuff;
		genLine())
	and run test = 
		let val tokenList= P.lexString(test()) in
			((P.printTokens tokenList);
			print (case (P.parse_stmt tokenList)
				of NONE=>"\n!\n"
				| SOME (stmt,ts)=> (I.strSt stmt)^"\n\n"))
			(*print (I.strSt (I.Block([I.Infix(I.Var("true"),"||",I.Var("false")),I.SmInitial(I.Default,"int","a")]))))*)
		end
			(*print (T.stringify (T.translate tokenList))*)


	fun testTrans()=
		let val SOME (stmt, ts) = (P.parse_stmt(P.lexString(genJava3())))
		in
			print ("\n"^(T.translate 0 stmt)^"\n\n")
		end

	(*Test that the file prints right*)
	fun testInput(file)=
		print(TextIO.inputAll(TextIO.openIn(file)))

	(*Print token stream from file*)
	fun tokenizeInput(file)=
		let val tokenList =P.lexString(TextIO.inputAll(TextIO.openIn(file)))
		in
			P.printTokens tokenList
		end

	(*Print parser output of file*)
	fun parseInput(file)=
		let val SOME (stmt, ts) = (P.parse_stmt(P.lexString(TextIO.inputAll(TextIO.openIn(file)))))
		in
			((P.printTokens ts);
				print (I.strSt stmt))
		end

	(*write file translation to a file*)
	fun transInput(fileIn,fileOut)=
		(*Put text content of input file through parser*)
		let val SOME (stmt, ts) = (P.parse_stmt(P.lexString(TextIO.inputAll(TextIO.openIn(fileIn)))))
		in
			(*(print (I.strSt stmt);
			print ("\n"^(T.translate 0 stmt)^"\n\n"))*)
			let val os = TextIO.openOut(fileOut)
			in
				(TextIO.output(os,(T.translate 0 stmt));
				(TextIO.closeOut(os)))
			end
		end
end