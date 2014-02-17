
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
 *
 *   There are tokens for all sample parsers
 *   Not all parsers use all tokens
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
 *   A SIMPLE PARSER FOR A SCHEME-SUBSET
 * 
 *   Grammar:
 * 
 *   expr ::= T_INT 
 *            T_TRUE
 *            T_FALSE
 *            T_SYM 
 *            T_LPAREN expr_seq T_RPAREN
 *
 *   expr_seq :: = T_LET T_LPAREN T_LPAREN T_SYM expr T_RPAREN T_RPAREN expr
 *                 T_IF expr expr expr
 *                 T_PLUS expr expr
 *                 T_TIMES expr expr
 *                 T_SYM expr
 *)


fun perror str = raise Fail ("parsing error: "^str)


fun expect_INT ((T_INT i)::ts) = (i,ts)
  | expect_INT _ = perror "expect_INT"

fun expect_TRUE (T_TRUE::ts) = ts
  | expect_TRUE _ = perror "expect_TRUE"

fun expect_FALSE (T_FALSE::ts) = ts
  | expect_FALSE _ = perror "expect_FALSE"

fun expect_SYM ((T_SYM s)::ts) = (s,ts)
  | expect_SYM _ = perror "expect_SYM"

fun expect_IF (T_IF::ts) = ts
  | expect_IF _ = perror "expect_IF"

fun expect_LET (T_LET::ts) = ts
  | expect_LET _ = perror "expect_LET"

fun expect_EQUAL (T_EQUAL::ts) = ts
  | expect_EQUAL _ = perror "expect_EQUAL"

fun expect_LPAREN (T_LPAREN::ts) = ts
  | expect_LPAREN _ = perror "expect_LPAREN"

fun expect_RPAREN (T_RPAREN::ts) = ts
  | expect_RPAREN _ = perror "expect_RPAREN"

fun expect_PLUS (T_PLUS::ts) = ts
  | expect_PLUS _ = perror "expect_PLUS"

fun expect_TIMES (T_TIMES::ts) = ts
  | expect_TIMES _ = perror "expect_TIMES"



fun parse_expr [] = perror "unexpected end of token sequence"
  | parse_expr ((T_INT i)::ts) = (EVal (VInt i),ts)
  | parse_expr (T_TRUE::ts) = (EVal (VBool true),ts)
  | parse_expr (T_FALSE::ts) = (EVal (VBool false),ts)
  | parse_expr ((T_SYM id)::ts) = (EIdent id, ts)
  | parse_expr (T_LPAREN::ts) = let
	val (e,ts) = parse_expr_seq ts
	val ts = expect_RPAREN ts
    in
	(e,ts)
    end
  | parse_expr _ = perror "unexpected token"



and parse_expr_seq [] = perror "unexpected end of token sequence"
  | parse_expr_seq (T_LET::ts) = let
	val ts = expect_LPAREN ts
	val ts = expect_LPAREN ts
	val (name,ts) = expect_SYM ts
	val (e1, ts) = parse_expr ts
	val ts = expect_RPAREN ts
	val ts = expect_RPAREN ts
	val (e2, ts) = parse_expr ts
    in
	(ELet (name,e1,e2),ts)
    end
  | parse_expr_seq (T_IF::ts) = let
	val (e1,ts) = parse_expr ts
	val (e2,ts) = parse_expr ts
	val (e3,ts) = parse_expr ts
    in
	(EIf (e1,e2,e3),ts)
    end
  | parse_expr_seq (T_PLUS::ts) = let
	val (e1,ts) = parse_expr ts
	val (e2,ts) = parse_expr ts
    in
	(EAdd (e1,e2),ts)
    end
  | parse_expr_seq (T_TIMES::ts) = let
	val (e1,ts) = parse_expr ts
	val (e2,ts) = parse_expr ts
    in
	(EMul (e1,e2),ts)
    end
  | parse_expr_seq (T_EQUAL::ts) = let
	val (e1,ts) = parse_expr ts
	val (e2,ts) = parse_expr ts
    in
	(EEq (e1,e2),ts)
    end
  | parse_expr_seq ((T_SYM name)::ts) = let
	val (e, ts) = parse_expr ts
    in
	(ECall (name,[e]),ts)
           (* the syntax forces us to use only one argument here *)
    end
  | parse_expr_seq _ = perror "unexpected token"

	

(* Top-level function to tokenize and parse a string *)

fun parse str = let
    val (e,_) = parse_expr (lexString str)
in
    e
end








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

(*val succ= (function "succ" ["n"] (EAdd (EIdent "n", EVal (VInt 1))))*)

val succ =("succ",
 FDef (["n"], 
 EAdd (EIdent "n", 
 EVal (VInt 1))))

val add3=("add3",
  FDef (["a","b","c"],
 EAdd (EAdd (EIdent "a", EIdent "b"), EIdent "c")))

(*eval [succ] (ECall ("succ",[(EVal (VInt 1))]));
*)
