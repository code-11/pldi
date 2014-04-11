structure Translator =  struct

	structure P = Parser

	fun tokenToPyString (P.T_SYM s) = s
    | tokenToPyString (P.T_INT i) = (Int.toString i)
    | tokenToPyString (P.T_STRING s) = s
    | tokenToPyString P.T_TRUE = "True"
    | tokenToPyString P.T_FALSE = "False"
    | tokenToPyString P.T_IF  = "if"
    | tokenToPyString P.T_THEN  = "then"
    | tokenToPyString P.T_ELSE  = "else"
    | tokenToPyString P.T_EQUAL = "=="
    | tokenToPyString P.T_LPAREN = "("
    | tokenToPyString P.T_RPAREN = ")"
    | tokenToPyString P.T_LBRACKET = "["
    | tokenToPyString P.T_RBRACKET = "]"
    | tokenToPyString P.T_PLUS = "+"
    | tokenToPyString P.T_TIMES = "*"
    | tokenToPyString P.T_COMMA = ","
    | tokenToPyString P.T_SEMICOLON = "\n"
    | tokenToPyString P.T_WHILE = "while"
    | tokenToPyString P.T_LBRACE = ":\n"
    | tokenToPyString P.T_RBRACE = "\n" (*ANTI TAB*)
    | tokenToPyString P.T_VAR = "T_VAR" (*shouldn't be used*)
    | tokenToPyString P.T_FOR = "for"
    | tokenToPyString P.T_ASSIGN = "="
    | tokenToPyString P.T_CLASS = "class"
    | tokenToPyString P.T_CONTINUE = "continue" (*Not sure*) 
    | tokenToPyString P.T_INSOF = "type"
    | tokenToPyString P.T_NEW = ""
    | tokenToPyString P.T_RETURN = "return"
    | tokenToPyString P.T_SUPER = "" (*Ignored for now*)
    | tokenToPyString P.T_THIS = "self"
    | tokenToPyString P.T_NULL = "None" (*Not a direct conversion*)
    | tokenToPyString P.T_DOT = "."
    | tokenToPyString P.T_FUNCSCOPE="def"
    | tokenToPyString P.T_STATIC=""
    | tokenToPyString P.T_INDENT="\t"
    | tokenToPyString _ ="NO TRANSLATION"


	(*takes in a list of tokens, returns a string*)
	(*Note to self: need to remember all defined types so we can get rid of them in variable declaration*)
	(*need to get rid of variable declaration that don't set the variable (e.g. int i;)*)

	fun dropUntilNewLineHelp (P.T_RBRACE::ts)= P.T_RBRACE::ts
		| dropUntilNewLineHelp (P.T_LBRACE::ts)= P.T_LBRACE::ts
		| dropUntilNewLineHelp (P.T_SEMICOLON::ts)= P.T_SEMICOLON::ts
		| dropUntilNewLineHelp (P.T_RPAREN::ts)= P.T_RPAREN::ts
		| dropUntilNewLineHelp (P.T_INDENT::ts)=P.T_INDENT::ts
		| dropUntilNewLineHelp (a::ts)=dropUntilNewLineHelp ts
		| dropUntilNewLineHelp [] = raise Match (*If its empty something messed up*)

	fun dropUntilNewLine (t::ts)=t::(dropUntilNewLineHelp ts)
		| dropUntilNewLine []=[]

	(*First argument: token list to walk down
	  Second argument: current sublist to add to - stuff between commas
	  Third argument: total list of lists that is being built up*)
	
	fun lasts tss=
		List.rev (List.map (fn x=> List.hd x) tss)

	fun doubleList [] currList toReturn= List.rev (currList::toReturn)
		| doubleList (P.T_COMMA::ts) currList toReturn= doubleList ts [] ([P.T_COMMA]::(currList::toReturn))
		| doubleList (t::ts) currList toReturn =doubleList ts (t::currList) toReturn

	fun isolateInputs (P.T_RPAREN::ts)= []
		| isolateInputs (t::ts)=t::(isolateInputs ts) 
		| isolateInputs _ =raise Match

	fun revIsolateInputs (P.T_RPAREN::ts)= P.T_RPAREN::ts
		| revIsolateInputs (t::ts)=(revIsolateInputs ts) 
		| revIsolateInputs _ =raise Match

(*	fun dropUntilCommaHelp (P.T_LPAREN::ts) = P.T_LPAREN::ts
		| dropUntilCommaHelp (P.T_COMMA::ts) = P.T_COMMA::ts
		| dropUntilCommaHelp (a::ts) = dropUntilCommaHelp ts
		| dropUntilCommaHelp []=[]

	fun dropUntilComma (t::ts)=t::(dropUntilCommaHelp ts)
		| dropUntilComma [] = []*)

	fun addIndents 0 ts = ts
		| addIndents x ts = P.T_INDENT::(addIndents (x-1) ts)

	fun translateHelp (P.T_ASSIGN::ts) level toReturn = (translateHelp ts level (P.T_ASSIGN::(dropUntilNewLine toReturn)))
		| translateHelp (P.T_LBRACE::ts) level toReturn = (translateHelp ts (level+1) (addIndents (level+1) (P.T_LBRACE::toReturn)))
		| translateHelp (P.T_SEMICOLON::ts) level toReturn = (translateHelp ts level (addIndents level (P.T_SEMICOLON::toReturn)))
		| translateHelp (P.T_SYM t::P.T_LPAREN::ts) level toReturn = translateHelp (revIsolateInputs ts) level ((lasts (doubleList (isolateInputs ts) [] []))@(P.T_LPAREN::(P.T_SYM t::toReturn)))
		| translateHelp (t::ts) level toReturn= (translateHelp ts level (t::toReturn))
		| translateHelp [] level toReturn=(List.rev toReturn)

	fun translate ts=translateHelp ts 0 []


	fun stringify (t::ts)= (tokenToPyString t)^" "^(stringify ts)
		|stringify []=""

end