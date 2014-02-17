
(* Code from Lecture 06 - Parsing *)



(*
 *   Wrapper around the regexp library
 * 
 *   See code for lecture 5 for details
 *)

CM.make "$/regexp-lib.cm";

structure R = RegExpFn (structure P = AwkSyntax structure E = DfaEngine)

(* match a compiled regular expression against a list of characters *)

fun matchRE' re cs = let
    val prefix = R.prefix re List.getItem
    fun getMatch NONE = NONE
      | getMatch (SOME (mt, cs')) = let
	    val {pos,len} = MatchTree.root mt
	in
	    SOME (implode (List.take (pos,len)), cs')
	end
in
    getMatch (prefix cs)
end

(* match a string regular expression against a list of characters *)

fun matchRE re cs = matchRE' (R.compileString re) cs




(* 
 *   A simple lexer
 *
 *   Details in lecture 5
 *
 *   Modified to deal with keywords correctly
 *)


datatype token = T_LET 
	       | T_SYM of string 
	       | T_INT of int 
	       | T_TRUE 
	       | T_FALSE
	       | T_PLUS
	       | T_TIMES
	       | T_EQUAL
	       | T_IF 
	       | T_THEN
	       | T_ELSE
	       | T_IN
               | T_COMMA
	       | T_LPAREN 
	       | T_RPAREN


fun whitespace _ = NONE

fun produceSymbol "let" = SOME (T_LET)
  | produceSymbol "true" = SOME (T_TRUE)
  | produceSymbol "false" = SOME (T_FALSE)
  | produceSymbol "if" = SOME (T_IF)
  | produceSymbol "then" = SOME (T_THEN)
  | produceSymbol "else" = SOME (T_ELSE)
  | produceSymbol "in" = SOME (T_IN)
  | produceSymbol text = SOME (T_SYM text)

fun produceInt text = SOME (T_INT (valOf (Int.fromString text)))

fun producePlus _ = SOME (T_PLUS)
fun produceTimes _ = SOME (T_TIMES)
fun produceEqual _ = SOME (T_EQUAL)
fun produceLParen _ = SOME (T_LPAREN)
fun produceRParen _ = SOME (T_RPAREN)
fun produceComma _ = SOME (T_COMMA)


val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
in
    map convert [("( |\\n|\\t)+",         whitespace),
		 ("\\+" ,                 producePlus),
		 ("\\*",                  produceTimes),
		 ("=",                    produceEqual),
                 (",",                    produceComma),
		 ("[a-zA-Z][a-zA-Z0-9]*", produceSymbol),
		 ("~?[0-9]+",             produceInt),
		 ("\\(",                  produceLParen),
		 ("\\)",                  produceRParen)]
end


fun getToken cs = let
    fun loop [] = raise Fail ("cannot tokenize "^(implode cs))
      | loop ((re,f)::xs) = (case matchRE' re cs of
				 NONE => loop xs
			       | SOME (m,cs') => (f m,cs'))
in
    loop tokens
end


fun lex []  = []
  | lex cs = let
	val (token,cs') = getToken cs
    in
        case token of 
          NONE => lex cs'
	| SOME t => t::(lex cs')
    end


fun lexString str = lex (explode str)







datatype value = VInt of int
               | VBool of bool

datatype expr = EVal of value
              | EAdd of expr * expr
              | EMul of expr * expr
	      | EEq of expr * expr
              | EIf of expr * expr * expr
              | ELet of string * expr * expr
              | EIdent of string
              | ECall of string * expr list






(* 
 *   A SIMPLE PARSER FOR AN ML-LIKE SYNTAX (MULTI-ARGUMENT FUNCTIONS)
 * 
 *   Grammar:
 * 
 *   expr ::= T_IF expr T_THEN expr T_ELSE expr  
 *            T_LET T_SYM T_EQUAL expr T_IN expr
 *            T_SYM T_LPAREN expr_list T_RPAREN
 *            eterm T_EQUAL eterm
 *            eterm
 *            
 *   expr_list :: = expr T_COMMA expr_list
 *                  expr
 *                  
 *   eterm ::= term T_PLUS term 
 *             term
 *
 *   term ::= factor T_TIMES factor
 *             factor
 * 
 *   factor ::= T_INT
 *             T_TRUE
 *             T_FALSE
 *             T_SYM
 *             T_LPAREN expr TRPAREN
 *            
 *
 *)

fun perror str = raise Fail ("parsing error: "^str)



fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
  | expect_INT _ = NONE

fun expect_TRUE (T_TRUE::ts) = SOME ts
  | expect_TRUE _ = NONE

fun expect_FALSE (T_FALSE::ts) = SOME ts
  | expect_FALSE _ = NONE

fun expect_SYM ((T_SYM s)::ts) = SOME (s,ts)
  | expect_SYM _ = NONE

fun expect_IF (T_IF::ts) = SOME ts
  | expect_IF _ = NONE

fun expect_THEN (T_THEN::ts) = SOME ts
  | expect_THEN _ = NONE

fun expect_ELSE (T_ELSE::ts) = SOME ts
  | expect_ELSE _ = NONE

fun expect_LET (T_LET::ts) = SOME ts
  | expect_LET _ = NONE

fun expect_EQUAL (T_EQUAL::ts) = SOME ts
  | expect_EQUAL _ = NONE

fun expect_IN (T_IN::ts) = SOME ts
  | expect_IN _ = NONE

fun expect_LPAREN (T_LPAREN::ts) = SOME ts
  | expect_LPAREN _ = NONE

fun expect_RPAREN (T_RPAREN::ts) = SOME ts
  | expect_RPAREN _ = NONE

fun expect_PLUS (T_PLUS::ts) = SOME ts
  | expect_PLUS _ = NONE

fun expect_TIMES (T_TIMES::ts) = SOME ts
  | expect_TIMES _ = NONE

fun expect_COMMA (T_COMMA::ts) = SOME ts
  | expect_COMMA _ = NONE

fun parse_expr ts = 
    (case parse_expr_1 ts
      of NONE => 
	 (case parse_expr_2 ts
	   of NONE => 
	      (case parse_expr_3 ts
		of NONE => 
		   (case parse_expr_4 ts
		     of NONE => parse_expr_5 ts
		      | s => s)
		 | s => s)
	    | s => s)
       | s => s)

					       
and parse_expr_1 ts = 
    (case expect_IF ts
      of NONE => NONE
       | SOME ts => 
	 (case parse_expr ts
	   of NONE => NONE
	    | SOME (e1,ts) => 
	      (case expect_THEN ts
		of NONE => NONE
		 | SOME ts => 
		   (case parse_expr ts
		     of NONE => NONE
		      | SOME (e2,ts) => 
			(case expect_ELSE ts
			  of NONE => NONE
			   | SOME ts => 
			     (case parse_expr ts
			       of NONE => NONE
				| SOME (e3,ts) => SOME (EIf (e1,e2,e3),ts)))))))

and parse_expr_2 ts = 
    (case expect_LET ts 
      of NONE => NONE
       | SOME ts => 
	 (case expect_SYM ts 
	   of NONE => NONE
	    | SOME (s,ts) => 
	      (case expect_EQUAL ts
		of NONE => NONE
		 | SOME ts => 
		   (case parse_expr ts
		     of NONE => NONE
		      | SOME (e1,ts) => 
			(case expect_IN ts
			  of NONE => NONE
			   | SOME ts => 
			     (case parse_expr ts
			       of NONE => NONE
				| SOME (e2,ts) => SOME (ELet (s,e1,e2),ts)))))))

and parse_expr_3 ts = 
    (case expect_SYM ts 
      of NONE => NONE
       | SOME (s, ts) => 
	 (case expect_LPAREN ts
	   of NONE => NONE
	    | SOME ts => 
	      (case parse_expr_list ts
		of NONE => NONE
		 | SOME (es,ts) => 
		   (case expect_RPAREN ts
		     of NONE => NONE
		      | SOME ts => SOME (ECall (s,es),ts)))))

and parse_expr_4 ts = 
    (case parse_eterm ts
      of NONE => NONE
       | SOME (e1,ts) => 
	 (case expect_EQUAL ts
	   of NONE => NONE
	    | SOME ts => 
	      (case parse_eterm ts
		of NONE => NONE
		 | SOME (e2,ts) => SOME (EEq (e1,e2),ts))))

and parse_expr_5 ts = parse_eterm ts



and parse_expr_list ts = 
    (case parse_expr_list_1 ts
      of NONE => parse_expr_list_2 ts
       | s => s)

and parse_expr_list_1 ts = 
    (case parse_expr ts
      of NONE => NONE
       | SOME (e,ts) => 
	 (case expect_COMMA ts
	   of NONE => NONE
	   | SOME ts => 
	     (case parse_expr_list ts
	       of NONE => NONE
		| SOME (es,ts) => SOME (e::es,ts))))

and parse_expr_list_2 ts = 
    (case parse_expr ts
      of NONE => NONE
       | SOME (e,ts) => SOME ([e],ts))



and parse_eterm ts = 
    (case parse_eterm_1 ts
      of NONE => parse_eterm_2 ts
       | s => s)

and parse_eterm_1 ts = 
    (case parse_term ts 
      of NONE => NONE
       | SOME (e1,ts) => 
	 (case expect_PLUS ts
	   of NONE => NONE
	    | SOME ts => 
	      (case parse_term ts
		of NONE => NONE
		| SOME (e2,ts) => SOME (EAdd (e1,e2),ts))))

and parse_eterm_2 ts = parse_term ts



and parse_term ts = 
    (case parse_term_1 ts
      of NONE => parse_term_2 ts
      | s => s)

and parse_term_1 ts = 
    (case parse_factor ts
      of NONE => NONE
       | SOME (e1,ts) => 
	 (case expect_TIMES ts
	   of NONE => NONE
	    | SOME ts => 
	      (case parse_factor ts
		of NONE => NONE
		| SOME (e2,ts) => SOME (EMul (e1,e2),ts))))

and parse_term_2 ts = parse_factor ts




and parse_factor ts = 
    (case parse_factor_1 ts
      of NONE =>
	 (case parse_factor_2 ts 
	   of NONE => 
	      (case parse_factor_3 ts 
		of NONE => 
		   (case parse_factor_4 ts
		     of NONE => parse_factor_5 ts
		      | s => s)
		 | s => s)
	    | s => s)
       | s => s)

and parse_factor_1 ts = 
    (case expect_INT ts 
      of NONE => NONE
       | SOME (i,ts) => SOME (EVal (VInt i),ts))

and parse_factor_2 ts = 
    (case expect_TRUE ts
      of NONE => NONE
       | SOME ts => SOME (EVal (VBool true),ts))

and parse_factor_3 ts = 
    (case expect_FALSE ts
      of NONE => NONE
       | SOME ts => SOME (EVal (VBool false),ts))

and parse_factor_4 ts = 
    (case expect_SYM ts
      of NONE => NONE
       | SOME (s,ts) => SOME (EIdent s,ts))

and parse_factor_5 ts = 
    (case expect_LPAREN ts
      of NONE => NONE
       | SOME ts =>
	 (case parse_expr ts
	   of NONE => NONE
	    | SOME (e,ts) => 
	      (case expect_RPAREN ts
		of NONE => NONE
		| SOME ts => SOME (e,ts))))

fun parse str = 
    (case parse_expr (lexString str)
      of SOME (e,[]) => e
       | _ => perror "Cannot parse expression")






(* 
 *    EVALUATION 
 * 
 *    Per previous lectures
 *
 *)

datatype function = FDef of string list * expr 

fun evalError msg = raise Fail ("evaluation error - "^msg)

fun applyAdd (VInt i) (VInt j) = VInt (i+j)
  | applyAdd _ _ = evalError "applyAdd"

fun applyMul (VInt i) (VInt j) = VInt (i*j)
  | applyMul _ _ = evalError "applyMul"

fun applyEq (VInt i) (VInt j) = VBool (i=j)
  | applyEq (VBool b) (VBool c) = VBool (b=c)
  | applyEq _ _ = evalError "applyAdd"



fun subst (EVal value) id v = EVal value
  | subst (EAdd (f,g)) id v = EAdd (subst f id v, subst g id v)
  | subst (EMul (f,g)) id v = EMul (subst f id v, subst g id v)
  | subst (EEq (f,g)) id v = EEq (subst f id v, subst g id v)
  | subst (EIf (f,g,h)) id v = EIf (subst f id v, 
                                    subst g id v,
                                    subst h id v)
  | subst (ELet (id',f,g)) id v = 
      if id = id'
      then ELet (id',subst f id v, g)
      else ELet (id',subst f id v, subst g id v)
  | subst (EIdent id') id v = if id = id'
                                then EVal v
                              else EIdent id'
  | subst (ECall (n,fs)) id v = ECall (n,substList fs id v)

and substList [] id v = []
  | substList (e::es) id v = (subst e id v)::(substList es id v)

fun substMulti e [] [] = e
  | substMulti e (id::ids) (v::vs) = substMulti (subst e id v) ids vs
  | substMulti _ _ _ = evalError "substMulti"

fun lookup (name:string) [] = evalError "lookup"
  | lookup name ((n,f)::fenv) = 
      if (n = name)
        then f
      else lookup name fenv 

fun eval _ (EVal v) = v
  | eval fenv (EAdd (e,f)) = applyAdd (eval fenv e) (eval fenv f)
  | eval fenv (EMul (e,f)) = applyMul (eval fenv e) (eval fenv f)
  | eval fenv (EEq (e,f)) = applyEq (eval fenv e) (eval fenv f)
  | eval fenv (EIf (e,f,g)) = evalIf fenv (eval fenv e) f g
  | eval fenv (ELet (name,e,f)) = evalLet fenv name e f
  | eval _ (EIdent _) = evalError "eval/EId"
  | eval fenv (ECall (name,es)) = 
                evalCall fenv (lookup name fenv) (evalList fenv es)

and evalList fenv [] = []
  | evalList fenv (e::es) = (eval fenv e)::(evalList fenv es)

and evalCall fenv (FDef (params,body)) vs = 
      eval fenv (substMulti body params vs)

and evalIf fenv (VBool true) f g = eval fenv f
  | evalIf fenv (VBool false) f g = eval fenv g
  | evalIf _ _ _ _ = evalError "evalIf"

and evalLet fenv id exp body = eval fenv (subst body id (eval fenv exp))





(* helper functions to use the interpreter *)

fun function name args body = (name, FDef (args, parse body))

fun evalString fenv str = eval fenv (parse str)

