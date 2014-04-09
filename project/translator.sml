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
    | tokenToPyString P.T_PLUS = "+"
    | tokenToPyString P.T_TIMES = "*"
    | tokenToPyString P.T_COMMA = ","
    | tokenToPyString P.T_SEMICOLON = "\n"
    | tokenToPyString P.T_WHILE = "while"
    | tokenToPyString P.T_LBRACE = ":\n\t"
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


	(*takes in a list of tokens, returns a string*)
	(*Note to self: need to remember all defined types so we can get rid of them in variable declaration*)
	(*need to get rid of variable declaration that don't set the variable (e.g. int i;)*)

	fun dropUntilNewLineHelp (P.T_RBRACE::ts)= P.T_RBRACE::ts
		| dropUntilNewLineHelp (P.T_LBRACE::ts)= P.T_LBRACE::ts
		| dropUntilNewLineHelp (P.T_SEMICOLON::ts)= P.T_SEMICOLON::ts
		| dropUntilNewLineHelp (P.T_RPAREN::ts)= P.T_RPAREN::ts
		| dropUntilNewLineHelp (a::ts)=dropUntilNewLineHelp ts
		(*If its empty something messed up*)

	fun dropUntilNewLine (t::ts)=(print "IN DROP\n";t::(dropUntilNewLineHelp ts))
		| dropUntilNewLine []=[]

	fun translate (P.T_ASSIGN::ts) = P.T_ASSIGN::(translate (dropUntilNewLine ts))
		| translate (t::ts)= t::(translate ts)
		| translate []=[]

	fun stringify (t::ts)= (tokenToPyString t)^" "^(stringify ts)
		|stringify []=""

end