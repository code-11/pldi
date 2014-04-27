 
 (* NOTE- The parser is less restrictive then the actual java parser so far
    Things like "if (a==b){String c="blah"}" are allowed. Note the lack of Semicolon.
 *)

structure Parser =  struct

  (*
   *  Wrapper around the regexp library
   *)      

  structure R = RegExpFn (structure P = AwkSyntax structure E = DfaEngine)

  structure I = InternalRepresentation
                
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



  exception Parsing of string

  fun parseError msg = raise Parsing msg

  (* 
   *   A simple lexer
   *
   *)

  datatype token = T_SYM of string 
                 | T_INT of int
                 | T_STRING of string
                 | T_COMMENT of string
                 | T_TRUE
                 | T_FALSE
                 | T_IF
		             | T_THEN
                 | T_ELSE
                 | T_EQUAL
                 | T_LPAREN 
                 | T_RPAREN
                 | T_RBRACKET
                 | T_LBRACKET
                 | T_PLUS
                 | T_TIMES
                 | T_COMMA
		             | T_SEMICOLON
		             | T_WHILE
		             | T_LBRACE
		             | T_RBRACE
		             | T_FUNCTION
		             | T_VAR
		             | T_FOR
                 | T_ASSIGN
                 | T_CLASS
                 | T_CONTINUE
                 | T_INSOF
                 | T_NEW
                 | T_RETURN
                 | T_SUPER
                 | T_THIS
                 | T_NULL
                 | T_DOT
                 | T_INFIX of string
                 | T_FUNCSCOPE of string
                 | T_STATIC
                 | T_INDENT

  fun stringOfToken (T_SYM s) = "T_SYM["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken (T_STRING s) = "T_STRING["^s^"]"
    | stringOfToken (T_COMMENT s)= "T_COMMENT["^s^"]"
    | stringOfToken T_TRUE = "T_TRUE"
    | stringOfToken T_FALSE = "T_FALSE"
    | stringOfToken T_IF  = "T_IF"
    | stringOfToken T_THEN  = "T_THEN"
    | stringOfToken T_ELSE  = "T_ELSE"
    | stringOfToken T_EQUAL = "T_EQUAL"
    | stringOfToken T_LPAREN = "T_LPAREN"
    | stringOfToken T_RPAREN = "T_RPAREN"
    | stringOfToken T_LBRACKET = "T_LBRACKET"
    | stringOfToken T_RBRACKET = "T_RBRACKET"
    | stringOfToken T_PLUS = "T_PLUS"
    | stringOfToken T_TIMES = "T_TIMES"
    | stringOfToken T_COMMA = "T_COMMA"
    | stringOfToken T_SEMICOLON = "T_SEMICOLON"
    | stringOfToken T_WHILE = "T_WHILE"
    | stringOfToken T_LBRACE = "T_LBRACE"
    | stringOfToken T_RBRACE = "T_RBRACE"
    | stringOfToken T_VAR = "T_VAR"
    | stringOfToken T_FOR = "T_FOR"
    | stringOfToken T_ASSIGN = "T_ASSIGN"
    | stringOfToken T_CLASS = "T_CLASS"
    | stringOfToken T_CONTINUE = "T_CONTINUE"
    | stringOfToken T_INSOF = "T_INSOF"
    | stringOfToken T_NEW = "T_NEW"
    | stringOfToken T_RETURN = "T_RETURN"
    | stringOfToken T_SUPER = "T_SUPER"
    | stringOfToken T_THIS = "T_THIS"
    | stringOfToken T_NULL = "T_NULL"
    | stringOfToken T_DOT = "T_DOT"
    | stringOfToken (T_INFIX s) ="T_INFIX["^s^"]"
    | stringOfToken (T_FUNCSCOPE s) ="T_FUNCSCOPE["^s^"]"
    | stringOfToken T_STATIC = "T_STATIC"
    | stringOfToken T_INDENT ="T_INDENT"
    | stringOfToken _="TOKEN_NOT_RECOGNIZED"

                   
  fun whitespace _ = NONE
                     
  (*all of the keywords of Java*)
  fun produceSymbol "true" = SOME (T_SYM "true")
    | produceSymbol "false" = SOME (T_SYM "false")
    | produceSymbol "if" = SOME (T_IF)
    | produceSymbol "then" = SOME (T_THEN)
    | produceSymbol "else" = SOME (T_ELSE)
    | produceSymbol "while" = SOME (T_WHILE)
    | produceSymbol "var" = SOME (T_VAR)
    | produceSymbol "for" = SOME (T_FOR)
    | produceSymbol "class" = SOME (T_CLASS)
    | produceSymbol "continue" = SOME (T_CONTINUE)
    | produceSymbol "instanceof" = SOME (T_INSOF)
    | produceSymbol "new" = NONE
    | produceSymbol "return" = SOME (T_RETURN)
    | produceSymbol "super" = SOME (T_SUPER)
    | produceSymbol "this" = SOME (T_THIS)
    | produceSymbol "null" = SOME (T_NULL)
    | produceSymbol "public" =SOME (T_FUNCSCOPE "public")
    | produceSymbol "private" =SOME (T_FUNCSCOPE "private")
    | produceSymbol "static" = NONE
    | produceSymbol text = SOME (T_SYM text)

  fun produceString text = SOME (T_STRING text)

  fun produceComment text=
    let val strList= String.explode text in
      SOME (T_COMMENT (implode (List.rev (List.tl (List.tl (List.rev (List.tl (List.tl strList))))))))
    end
                           
  fun produceInt text = (case Int.fromString text
                          of NONE => parseError "integer literal out of bounds"
                           | SOME i => SOME (T_INT i))
                        
  fun produceEqual _ = SOME (T_INFIX "==")
  fun produceLParen _ = SOME (T_LPAREN)
  fun produceRParen _ = SOME (T_RPAREN)

  fun produceLBrace _ = SOME (T_LBRACE)
  fun produceRBrace  _ = SOME (T_RBRACE)

  fun produceLBracket _=SOME (T_LBRACKET)
  fun produceRBracket _=SOME (T_RBRACKET)

  fun produceInfix s = SOME (T_INFIX s)

  fun producePlus _ = SOME (T_INFIX "+")
  fun produceTimes _ = SOME (T_INFIX "*")
  fun produceMinus _ =SOME (T_INFIX "-")
  fun produceDiv _ =SOME (T_INFIX "/") 
  fun produceLess _ =SOME (T_INFIX "<")
  fun produceGreat _ = SOME (T_INFIX ">")
  fun produceLessEq _=SOME (T_INFIX "<=")
  fun produceGreatEq _ = SOME (T_INFIX ">=")
  fun produceAnd _= SOME (T_INFIX "&&")
  fun produceOr _= SOME (T_INFIX "||")
  fun produceComma _ = SOME (T_COMMA)
  fun produceAssign _ = SOME (T_ASSIGN)
  fun produceDot _= SOME (T_INFIX ".")

  fun producePlusAssign _ = SOME(T_INFIX "+=")
  fun produceMinusAssign _ =SOME(T_INFIX "-=")
  fun produceTimesAssign _ =SOME(T_INFIX "*=")
  fun produceDivAssign _ = SOME (T_INFIX "/=")

  fun produceSemiColon _ = SOME (T_SEMICOLON)
  
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",           whitespace),
                 ("\\/\\*[^\\*]*\\*\\/", produceComment),
                 ("\\+=",            producePlusAssign),
                 ("\\-=",           produceMinusAssign),
                 ("\\*=",           produceTimesAssign),
                 ("==",                   produceEqual),
                 ("\\+",                   producePlus),
                 ("\\-",                  produceMinus),
            		 ("\\*",                  produceTimes),
                 ("\\/",                    produceDiv),
                 ("<",                     produceLess),
                 (">",                    produceGreat),
                 ("<=",                  produceLessEq),
                 (">=",                 produceGreatEq),
                 ("&&",                     produceAnd),
                 ("\\|\\|",                  produceOr),
            		 (",",                    produceComma),
            		 (";",                    produceSemiColon),
                 ("[a-zA-Z][\\[\\]a-zA-Z0-9\\.]*", produceSymbol),
                 ("~?[0-9]+",             produceSymbol),
                 ("\\(",                  produceLParen),
                 ("\\)",                  produceRParen),
                 ("{",                    produceLBrace),
                 ("}",                    produceRBrace),
                 ("=",                    produceAssign),
                 ("\\[",                  produceLBracket),
                 ("\\]",                  produceRBracket),
                 ("\\.",                  produceDot),
                 ("\\\"[^\\\"]*\\\"",     produceSymbol)]
  end
  
  fun expect token (t::ts) = if t=token then SOME ts else NONE
    | expect _ _ = NONE

  fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
    | expect_INT _ = NONE

  fun expect_SYM ((T_SYM s)::ts) = SOME (s,ts)
    | expect_SYM _ = NONE

  fun expect_INFIX ((T_INFIX s)::ts) = SOME (s,ts)
    | expect_INFIX _ = NONE       

  fun expect_SCOPE ((T_FUNCSCOPE s)::ts)= SOME (s,ts)
    | expect_SCOPE _=NONE   

  fun expect_COMMENT ((T_COMMENT s)::ts) = SOME (s,ts)
    | expect_COMMENT _=NONE
               
  fun getToken cs = let
    fun loop [] = parseError ("cannot tokenize "^(implode cs))
      | loop ((re,f)::xs) = (case matchRE' re cs
                              of NONE => loop xs
                               | SOME (m,cs') => (f m,cs'))
  in
    loop tokens
  end
                    
                    
  fun lex []  = []
    | lex cs = let
        val (token,cs') = getToken cs
      in
        case token 
         of NONE => lex cs'
          | SOME t => t::(lex cs')
      end      
               
  fun lexString str = lex (explode str)

  fun printToken token= (print (stringOfToken token); print "\n")

  fun printTokens [] = ()
     | printTokens (x::xs)=  ((printToken x); (printTokens xs))

  fun choose [] ts = NONE
    | choose (p::ps) ts = 
        (case p ts
    of NONE => choose ps ts
     | s => s)

(*find the array*)
(*takes a string and a token list*)
(*Returns the string with [] appended to it for each array it finds*)
(*If there's an unmatched LBRACKET< it fails.*)
(*
fun find_array s ts = 
	(case expect T_LBRACKET ts
		of NONE => SOME (s,ts)
		 | SOME ts =>
		 (case expect T_RBRACKET ts
		 	of NONE => NONE
		 	 | SOME ts => find_array (s^"[]") ts))
*)

  fun parse_expr ts=let 

    fun parse_eparen ts=
      (case expect T_LPAREN ts
        of NONE=>NONE
        | SOME ts=>
        (case parse_stmt ts
          of NONE=>NONE
          | SOME (stmt,ts)=>
          (case expect T_RPAREN ts
            of NONE=>NONE
            | SOME ts=> SOME (I.EParen(stmt),ts))))

    fun parse_evar ts=
      (case expect_SYM ts
        of NONE=>NONE
        | SOME (s,ts)=>(SOME (I.EVar(s),ts)))

    fun parse_ecall ts=
      (case expect_SYM ts
        of NONE=>NONE
        | SOME (name,ts)=>
        (case expect T_LPAREN ts
          of NONE=>NONE
          | SOME ts=>
          (case parse_inputs ts
            of NONE=>NONE
            | SOME (args,ts)=>
            (case expect T_RPAREN ts
              of NONE=>NONE
              | SOME ts=>SOME (I.ECall(name,args),ts)))))


    in     choose [parse_eparen,parse_ecall,parse_evar] ts
  end

  and parse_stmt ts=let

    fun parse_array ts =
      (case expect T_LBRACKET ts
        of NONE => NONE
         | SOME ts =>
         (case parse_inputs ts
          of NONE => NONE
           | SOME (ss,ts) =>
            (case expect T_RBRACKET ts
              of NONE => NONE
               | SOME ts => SOME (I.ArrLit(ss),ts))))

    fun parse_call ts=
      (case expect_SYM ts
        of NONE=>NONE
        | SOME (name,ts)=>
        (case expect T_LPAREN ts
          of NONE=>NONE
          | SOME ts=>
          (case parse_inputs ts
            of NONE=>NONE
            | SOME (args,ts)=>
            (case expect T_RPAREN ts
              of NONE=>NONE
              | SOME ts=>
              (case expect T_SEMICOLON ts
                of NONE=>NONE
                | SOME ts=> SOME (I.Call(name,args),ts))))))

    fun parse_paren ts=
      (case expect T_LPAREN ts
        of NONE=>NONE
        | SOME ts=>
        (case parse_stmt ts
          of NONE=>NONE
          | SOME (stmt,ts)=>
          (case expect T_RPAREN ts
            of NONE=>NONE
            | SOME ts=> SOME (I.Paren(stmt),ts))))

    fun parse_var ts=
      (case expect_SYM ts
        of NONE=>NONE
        | SOME (s,ts)=>(SOME (I.Var(s),ts)))

    fun parse_comment ts=
      (case expect_COMMENT ts
        of NONE=>NONE
        | SOME (text,ts)=>SOME (I.Comment(text),ts))

    fun parse_infix ts=
         (case parse_expr ts
            of NONE=>NONE
             | SOME (val1,ts)=>
             (case expect_INFIX ts
               of NONE=>NONE
                | SOME (oprtr,ts)=>
                (case parse_stmt ts
                  of NONE=>NONE
                  | SOME (val2,ts)=>SOME (I.Infix(val1,oprtr,val2),ts))))


    fun parse_meth_def ts=
      (case parse_scope ts
        of NONE=>NONE
        | SOME (sc,ts)=>
        (case expect_SYM ts
          of NONE=>NONE
          | SOME (retype,ts)=>
          (case expect_SYM ts
            of NONE=>NONE
             | SOME (name, ts) =>
            (case expect T_LPAREN ts
              of NONE=>NONE
              | SOME ts=>
              (case parse_meth_def_args ts
                of NONE=>NONE 
                | SOME (args,ts)=>
                (case expect T_RPAREN ts
                  of NONE=>NONE
                  | SOME ts=>
                  (case parse_stmt ts
                    of NONE=>NONE
                    | SOME (stmt,ts)=>SOME (I.MethDef(sc,retype,name,args,stmt),ts))))))))

   fun parse_return ts=
      (case expect T_RETURN ts
        of NONE=>NONE
        | SOME ts=>
        (case parse_stmt ts
          of NONE=>NONE
          | SOME (s,ts)=> 
            (case expect T_SEMICOLON ts
              of NONE=>NONE
              |SOME ts=>SOME (I.Return(s),ts))))

    fun parse_while ts=
      (case expect T_WHILE ts
        of NONE=> NONE
        | SOME ts=>
        (case expect T_LPAREN ts
          of NONE=> NONE
          | SOME ts=>
          (case parse_stmt ts
            of NONE=> NONE
            | SOME (s,ts)=>
            (case expect T_RPAREN ts
              of NONE=> NONE
              | SOME ts=>
              (case parse_stmt ts 
                of NONE=> NONE
                | SOME (stmt,ts)=>SOME (I.While(s,stmt),ts))))))

    fun parse_assign ts=
      (case expect_SYM ts
        of NONE=>NONE
        |SOME (s1,ts)=>
        (case expect T_ASSIGN ts
          of NONE=>NONE
          | SOME ts=>
          (case parse_stmt ts
            of NONE=>NONE
            |SOME (s2,ts)=> 
              (case expect T_SEMICOLON ts
                of NONE=>NONE
                | SOME ts=> SOME(I.Assign(s1,s2),ts)))))

    fun parse_if ts=
      (case expect T_IF ts
        of NONE=>NONE
        | SOME ts=>
        (case expect T_LPAREN ts
          of NONE=>NONE
          |SOME ts=>
          (case parse_stmt ts
            of NONE=>NONE
            | SOME (s,ts)=>
            (case expect T_RPAREN ts
              of NONE=>NONE
              | SOME ts=>
              (case parse_stmt ts
                of NONE=>NONE 
                | SOME (stmt,ts)=>
                (case expect T_ELSE ts
                  of NONE=> SOME (I.If(s,stmt),ts)
                  | SOME ts=>
                  (case parse_stmt ts
                    of NONE=> NONE
                    | SOME (stmt2,ts)=> SOME (I.IfElse(s,stmt,stmt2),ts))))))))

    fun parse_block ts=
      (case expect T_LBRACE ts
        of NONE=>NONE
        | SOME ts=>
        (case parse_stmt_list ts
          of NONE=>
            (case expect T_RBRACE ts
              of NONE=>NONE
              | SOME ts=> SOME (I.Block([]),ts)) 
          | SOME (ss,ts)=>
          (case expect T_RBRACE ts
            of NONE=>NONE
            | SOME ts=> SOME (I.Block(ss),ts))))

    fun parse_initial ts=
      (case parse_scope ts
        of NONE=>NONE  
        | SOME (sc,ts)=>
          (case expect_SYM ts
            of NONE=>NONE
            | SOME (s1,ts)=>
            (case expect_SYM ts
              of NONE=>NONE
              | SOME (s2,ts)=>
              (case expect T_ASSIGN ts
                of NONE=>
                  (case expect T_SEMICOLON ts
                    of NONE=>NONE
                    | SOME ts=> SOME (I.SmInitial(sc,s1,s2),ts))
                | SOME ts=>
                (case parse_stmt ts
                  of NONE=>NONE
                  | SOME (s3,ts)=> 
                  (case expect T_SEMICOLON ts
                    of NONE=>NONE
                    | SOME ts=> SOME (I.Initial(sc,s1,s2,s3),ts)))))))
    
    fun parse_class_def ts=
      (case parse_scope ts
        of NONE=>NONE
        | SOME (sc,ts)=>
        (case expect T_CLASS ts
          of NONE=>NONE
          | SOME ts=>
          (case expect_SYM ts
            of NONE=>NONE
            | SOME (s,ts)=>
            (case parse_stmt ts
              of NONE=>NONE
              | SOME (stmt,ts)=> SOME (I.ClassDef(sc,s,stmt),ts)))))
  in 
    choose [parse_array,parse_paren,parse_if,parse_call,parse_meth_def,parse_return,parse_while,parse_assign,parse_block,parse_infix,parse_class_def,parse_initial,parse_comment,parse_var] ts
  end
    
  and parse_scope ts=
    (case expect_SCOPE ts 
      of NONE=> SOME (I.Default,ts)
      | SOME (s,ts)=>(case s 
        of "public"=> SOME (I.Public,ts)
        | "private"=> SOME (I.Private,ts)
        | _=>NONE)) 

  and parse_stmt_list ts=
    (case parse_stmt ts
      of NONE=>NONE
      | SOME (stmt,ts)=>
        (case parse_stmt_list ts
          of NONE=>SOME ([stmt],ts)
          | SOME (ss,ts)=> SOME (stmt::ss,ts)))

  and parse_meth_def_args ts=
    (case expect_SYM ts
      of NONE=>SOME ([],ts)
      | SOME (typ,ts)=>
         (case expect_SYM ts
        	of NONE=>NONE
        	 | SOME (name,ts)=>
         	 (case expect T_COMMA ts
          	   of NONE=> SOME ([(typ,name)],ts)
          		| SOME ts=>
          		(case parse_meth_def_args ts
            	  of NONE=>NONE
            	  | SOME (args,ts)=> SOME ((typ,name)::args,ts)))))
  
  and parse_inputs ts=
    (case parse_stmt ts
      of NONE=>SOME ([],ts)
      | SOME (s,ts)=>
      (case expect T_COMMA ts
        of NONE=>SOME([s],ts)
        | SOME ts=>
        (case parse_inputs ts
          of NONE=>NONE
          | SOME (args,ts)=> SOME(s::args,ts))))

  end


