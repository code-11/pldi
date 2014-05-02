 
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
                 | T_NOT
                 | T_DOT
                 | T_SINFIX of string
                 | T_EINFIX of string
                 | T_FUNCSCOPE of string
                 | T_STATIC
                 | T_INDENT
                 | T_DPLUS
                 | T_DMINUS

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
    | stringOfToken T_NOT ="T_NOT"
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
    | stringOfToken (T_SINFIX s) ="T_SINFIX["^s^"]"
    | stringOfToken (T_EINFIX s) ="T_EINFIX["^s^"]"
    | stringOfToken (T_FUNCSCOPE s) ="T_FUNCSCOPE["^s^"]"
    | stringOfToken T_STATIC = "T_STATIC"
    | stringOfToken T_INDENT ="T_INDENT"
    | stringOfToken T_DPLUS = "T_DPLUS"
    | stringOfToken T_DMINUS ="T_DMINUS"
    | stringOfToken _="TOKEN_NOT_RECOGNIZED"


  (*A mini-translator of tokens for use in array symbols*)
  fun repStringOfToken(T_SYM s) = s
    | repStringOfToken (T_INT i) = Int.toString i
    | repStringOfToken (T_STRING s) = s
    | repStringOfToken (T_COMMENT s)= s
    | repStringOfToken T_TRUE = "True"
    | repStringOfToken T_FALSE = "False"
    | repStringOfToken T_IF  = "if"
    | repStringOfToken T_THEN  = "then"
    | repStringOfToken T_ELSE  = "else"
    | repStringOfToken T_EQUAL = "=="
    | repStringOfToken T_LPAREN = "("
    | repStringOfToken T_RPAREN = ")"
    | repStringOfToken T_LBRACKET = "["
    | repStringOfToken T_RBRACKET = "]"
    | repStringOfToken T_NOT ="not"
    | repStringOfToken T_PLUS = "+"
    | repStringOfToken T_TIMES = "*"
    | repStringOfToken T_COMMA = ","
    | repStringOfToken T_SEMICOLON = ""
    | repStringOfToken T_WHILE = "while"
    | repStringOfToken T_LBRACE = "{"
    | repStringOfToken T_RBRACE = "}"
    | repStringOfToken T_VAR = "var"
    | repStringOfToken T_FOR = "for"
    | repStringOfToken T_ASSIGN = "="
    | repStringOfToken T_CLASS = "class"
    | repStringOfToken T_CONTINUE = "continue"
    | repStringOfToken T_INSOF = "type"
    | repStringOfToken T_NEW = ""
    | repStringOfToken T_RETURN = "return"
    | repStringOfToken T_SUPER = "super"
    | repStringOfToken T_THIS = "self"
    | repStringOfToken T_NULL = "None"
    | repStringOfToken T_DOT = "."
    | repStringOfToken (T_SINFIX s) =s
    | repStringOfToken (T_EINFIX s) =s
    | repStringOfToken (T_FUNCSCOPE s) =""
    | repStringOfToken T_STATIC = ""
    | repStringOfToken T_INDENT ="\t"
    | repStringOfToken T_DPLUS = "++"
    | repStringOfToken T_DMINUS ="--"
    | repStringOfToken _=""

                   
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
(*    | produceSymbol "null" = NONE*)
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
                        
  
  fun produceLParen _ = SOME (T_LPAREN)
  fun produceRParen _ = SOME (T_RPAREN)

  fun produceLBrace _ = SOME (T_LBRACE)
  fun produceRBrace  _ = SOME (T_RBRACE)

  fun produceLBracket _=SOME (T_LBRACKET)
  fun produceRBracket _=SOME (T_RBRACKET)

  fun produceSInfix s = SOME (T_SINFIX s)
  fun produceEInfix s = SOME (T_EINFIX s)

  fun produceNot _= SOME (T_NOT)

  fun producePlus _ = SOME (T_EINFIX "+")
  fun produceTimes _ = SOME (T_EINFIX "*")
  fun produceMinus _ =SOME (T_EINFIX "-")
  fun produceDiv _ =SOME (T_EINFIX "/") 
  fun produceLess _ =SOME (T_EINFIX "<")
  fun produceGreat _ = SOME (T_EINFIX ">")
  fun produceLessEq _=SOME (T_EINFIX "<=")
  fun produceGreatEq _ = SOME (T_EINFIX ">=")
  fun produceEqual _ = SOME (T_EINFIX "==")
  fun produceNotEqual _ = SOME (T_EINFIX "!=")
  fun produceAnd _= SOME (T_EINFIX "&&")
  fun produceOr _= SOME (T_EINFIX "||")
  fun produceComma _ = SOME (T_COMMA)
  fun produceAssign _ = SOME (T_ASSIGN)
  fun produceDot _= SOME (T_EINFIX ".")
  fun produceDPlus _ = SOME (T_DPLUS)
  fun produceDMinus _ = SOME (T_DMINUS)

  fun producePlusAssign _ = SOME(T_SINFIX "+=")
  fun produceMinusAssign _ =SOME(T_SINFIX "-=")
  fun produceTimesAssign _ =SOME(T_SINFIX "*=")
  fun produceDivAssign _ = SOME (T_SINFIX "/=")

  fun produceSemiColon _ = SOME (T_SEMICOLON)
  
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",           whitespace),
                 ("\\/\\*[^\\*]*\\*\\/",produceComment),
                 ("\\-\\-",              produceDMinus),
                 ("\\+\\+",               produceDPlus),
                 ("\\+=",            producePlusAssign),
                 ("\\-=",           produceMinusAssign),
                 ("\\*=",           produceTimesAssign),
                 ("==",                   produceEqual),
                 ("!=",                produceNotEqual),
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
            		 (";",                produceSemiColon),
                 ("[a-zA-Z][a-zA-Z0-9\\.]*", produceSymbol),
                 ("~?[0-9]+",             produceSymbol),
                 ("\\(",                  produceLParen),
                 ("\\)",                  produceRParen),
                 ("{",                    produceLBrace),
                 ("}",                    produceRBrace),
                 ("=",                    produceAssign),
                 ("\\[",                  produceLBracket),
                 ("\\]",                  produceRBracket),
                 ("\\.",                  produceDot),
                 ("!",                      produceNot),
                 ("\\\"[^\\\"]*\\\"",     produceSymbol)]
  end
  
  fun expect token (t::ts) = if t=token then SOME ts else NONE
    | expect _ _ = NONE

  fun expect_INT ((T_INT i)::ts) = SOME (i,ts)
    | expect_INT _ = NONE

  fun expect_SINFIX ((T_SINFIX s)::ts) = SOME (s,ts)
    | expect_SINFIX _ = NONE     

  fun expect_EINFIX ((T_EINFIX s)::ts) = SOME (s,ts)
    | expect_EINFIX _ = NONE    

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

  fun repStringOfTokens [] = ""
    | repStringOfTokens (x::xs) = (repStringOfToken x)^(repStringOfTokens xs)

  fun printToken token= (print (stringOfToken token); print "\n")

  fun printTokens [] = ()
     | printTokens (x::xs)=  ((printToken x); (printTokens xs))

  fun choose [] ts = NONE
    | choose (p::ps) ts = 
        (case p ts
    of NONE => choose ps ts
     | s => s)

  (*Utility function that pulls out the tokens that are different between two token lists*)
  (*First argument is the longer one*)
  (*third argument is the list built up internally*)
  fun diff_token (t1::ts1) ts2 ret =
    if (ts1 = ts2) then (t1::ret) else (diff_token ts1 ts2 (t1::ret))
    | diff_token [] [] ret = ret
    | diff_token _ _ _ = parseError "unexpected tokens"

  (*incorporate parse_sym_array because almost any symbol could have []
    (and the ones without will be caught by Java)*)
  (*needs to be down here because it's dependent on parses....*)
  fun expect_SYM ((T_SYM s)::ts) = 
      (case parse_sym_array ts s
        of NONE => SOME (s,ts)
         | SOME (s,ts) => (SOME (s,ts)))
    | expect_SYM _ = NONE

  and parse_limExpr ts=let 

    fun parse_neg ts=
      (case expect_EINFIX ts
        of NONE=>NONE
        | SOME (infx,ts)=>
          (case infx of 
            "-"=>
              (case parse_expr ts
                of NONE=>NONE
                | SOME (expr,ts)=> SOME ((I.Neg expr),ts))
             | _ =>NONE))

    fun parse_not ts=
      (case expect T_NOT ts
        of NONE=>NONE
        | SOME ts=>
        (case parse_expr ts
          of NONE=>NONE
          | SOME (expr,ts)=>SOME (I.Not(expr),ts)))

    fun parse_eparen ts=
      (case expect T_LPAREN ts
        of NONE=>NONE
        | SOME ts=>
        (case parse_expr ts
          of NONE=>NONE
          | SOME (expr,ts)=>
          (case expect T_RPAREN ts
            of NONE=>NONE
            | SOME ts=> SOME (I.Paren(expr),ts))))

    fun parse_evar ts=
      (case expect_SYM ts
        of NONE=>NONE
        | SOME (s,ts)=> SOME (I.Var(s),ts))

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

      fun parse_array ts =
      (case expect T_LBRACE ts
        of NONE => NONE
         | SOME ts =>
         (case parse_inputs ts
          of NONE => NONE
           | SOME (ss,ts) =>
            (case expect T_RBRACE ts
              of NONE => NONE
               | SOME ts => SOME (I.ArrLit(ss),ts))))


    in     choose [parse_array,parse_eparen,parse_ecall,parse_not,parse_neg,parse_evar] ts
  end

  and parse_expr ts= let
    (*Parse infix operators that return a value and do NOT require a semicolon*)
    fun parse_Einfix ts=
         (case parse_limExpr ts
            of NONE=>NONE
             | SOME (val1,ts)=>
             (case expect_EINFIX ts
               of NONE=>NONE
                | SOME (oprtr,ts)=>
                (case parse_expr ts
                  of NONE=> NONE
                  | SOME (val2,ts)=>SOME (I.EInfix(val1,oprtr,val2),ts))))

    fun parse_other_expr ts=
      (case parse_limExpr ts
        of NONE=>NONE
        | SOME (expr,ts)=> SOME ((I.LExpr(expr)),ts) )
    in  
      choose [parse_Einfix,parse_other_expr] ts
    end


  and parse_stmt ts=let

(*Special case function for when infix operators who normally need a semicolon don't*)
    fun special_Sinfix ts=
          (case parse_expr ts
            of NONE=>NONE
             | SOME (val1,ts)=>
             (case expect_SINFIX ts
               of NONE=>NONE
                | SOME (oprtr,ts)=>
                (case parse_stmt ts
                  of NONE=> NONE
                  | SOME (val2,ts)=>SOME (I.SInfix(val1,oprtr,val2),ts))))


    fun parse_for3 ts=
      (case expect T_FOR ts
        of NONE=>NONE
        | SOME ts=>
        (case expect T_LPAREN ts
          of NONE=>NONE
          | SOME ts=>
          (case parse_stmt ts
            of NONE=>NONE
            | SOME (init,ts)=>
            (case parse_expr ts
              of NONE=>NONE
              | SOME (check,ts)=>
              (case expect T_SEMICOLON ts
                of NONE=>NONE
                | SOME ts=>
                  (case special_Sinfix ts
                    of SOME (infx,ts)=>
                      (case expect T_RPAREN ts
                        of NONE=>NONE
                        | SOME ts=>
                        (case parse_stmt ts
                          of NONE=>NONE
                          | SOME (rest,ts)=>SOME (I.For3(init,check,infx,rest),ts)))
                    | NONE=>
                      (case parse_stmt ts
                        of NONE=>NONE
                        | SOME (stmt,ts)=>
                        (case expect T_RPAREN ts
                          of NONE=>NONE
                          | SOME ts=>
                          (case parse_stmt ts
                            of NONE=>NONE
                            | SOME (rest,ts)=>SOME (I.For3(init,check,stmt,rest),ts))))))))))

    fun parse_scall ts=
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
                | SOME ts=> SOME (I.SCall(name,args),ts))))))

    fun parse_comment ts=
      (case expect_COMMENT ts
        of NONE=>NONE
        | SOME (text,ts)=>
          (case ts of
            [] => SOME (I.Comment(text,I.Block([])),ts)
            | _ =>
              (case parse_stmt ts
                of NONE => NONE
                 | SOME (_, _) =>
                    (let val SOME (stmt, ts) = (parse_stmt ts)
                    in 
                      SOME (I.Comment(text,stmt),ts)
                    end))))

(*Parse infix operators that usually don't return a value and DO require a semicolon*)
    fun parse_Sinfix ts=
         (case parse_expr ts
            of NONE=>NONE
             | SOME (val1,ts)=>
             (case expect_SINFIX ts
               of NONE=>NONE
                | SOME (oprtr,ts)=>
                (case parse_stmt ts
                  of NONE=> NONE
                  | SOME (val2,ts)=>
                  (case expect T_SEMICOLON ts
                    of NONE=>NONE
                    | SOME ts=>SOME (I.SInfix(val1,oprtr,val2),ts)))))


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
        (case parse_expr ts
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
          (case parse_expr ts
            of NONE=> NONE
            | SOME (s,ts)=>
            (case expect T_RPAREN ts
              of NONE=> NONE
              | SOME ts=>
              (case parse_stmt ts 
                of NONE=> NONE
                | SOME (stmt,ts)=>SOME (I.While(s,stmt),ts))))))

(*note: ignoring ++a and i=j++; and the like for now - requires more backtracking than we'd like*)
    fun parse_postfix ts=
      (case expect_SYM ts
        of NONE => NONE
         | SOME (s,ts) =>
            (case expect T_DPLUS ts
              of NONE => 
                (case expect T_DMINUS ts
                  of NONE=>NONE
                  | SOME ts=>
                    (case expect T_SEMICOLON ts
                      of NONE=>NONE
                      | SOME ts => SOME (I.SInfix((I.LExpr(I.Var s)),"-=",I.CheatExpr(I.LExpr(I.Var "1"))),ts)))
               | SOME ts =>
                 (case expect T_SEMICOLON ts
                  of NONE=>NONE
                    | SOME ts => SOME (I.SInfix((I.LExpr(I.Var s)),"+=",I.CheatExpr(I.LExpr(I.Var "1"))),ts) )))

(*For special cases, when a ++ or -- posfix is found without a semicolon*)
   fun special_postfix ts=
    (case expect_SYM ts
      of NONE => NONE
       | SOME (s,ts) =>
          (case expect T_DPLUS ts
            of NONE => 
              (case expect T_DMINUS ts
                of NONE=>NONE
                | SOME ts=> SOME (I.SInfix((I.LExpr(I.Var s)),"-=",I.CheatExpr(I.LExpr(I.Var "1"))),ts))
             | SOME ts => SOME (I.SInfix((I.LExpr(I.Var s)),"+=",I.CheatExpr(I.LExpr(I.Var "1"))),ts)))


    fun parse_assign ts=
      (case expect_SYM ts
        of NONE=>NONE
        |SOME (s1,ts)=>
        (case expect T_ASSIGN ts
          of NONE=>NONE
          | SOME ts=>
          (case parse_expr ts
            of NONE=> NONE
            |SOME (s2,ts)=> 
              (case expect T_SEMICOLON ts
                of NONE=> NONE
                | SOME ts=> SOME(I.Assign(s1,s2),ts)))))

    fun parse_if ts=
      (case expect T_IF ts
        of NONE=>NONE
        | SOME ts=>
        (case expect T_LPAREN ts
          of NONE=>NONE
          |SOME ts=>
          (case parse_expr ts
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
                    (case parse_expr ts
                      of NONE=>NONE 
                      | SOME (expr,ts)=> 
                        (case expect T_SEMICOLON ts
                          of NONE=>NONE
                          | SOME ts=> SOME (I.Initial(sc,s1,s2,expr),ts)))))))
    
    fun parse_cheat_expr ts=
      (case parse_expr ts
        of NONE=>NONE
        | SOME (expr,ts)=> SOME (I.CheatExpr(expr),ts))

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
    choose [parse_postfix,parse_for3,parse_if,parse_scall,parse_meth_def,parse_return,parse_while,parse_assign,parse_block,parse_Sinfix,parse_class_def,parse_initial,parse_comment,parse_cheat_expr] ts
  end

  (*array initialization/access*)
  (*Not really an expression or a statement - is essentially part of the symbol*)
  (*That's why it takes a string - that's the symbol*)
  (*However, has rules that need to be accounted for*)
  (*Returns the symbol with the array stuff appended and remaining tokens*)
  (*Probably not best approach, but should be workable*)
  and parse_sym_array ts st = 
    (case expect T_LBRACKET ts
      of NONE => NONE
       | SOME ts1 =>
       (case parse_expr ts1
          of NONE =>
            (*test for empty array*)
             (case expect T_RBRACKET ts1
                of NONE => NONE
                | SOME ts => SOME ((st^"[]"),ts))
           | SOME (e,ts) => 
            (case expect T_RBRACKET ts
              of NONE => NONE
               | SOME ts2 => (SOME ((st^"["^(repStringOfTokens (diff_token ts1 ts2 []))),ts2)))))
    
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
    (case parse_expr ts
      of NONE=>SOME ([],ts)
      | SOME (s,ts)=>
      (case expect T_COMMA ts
        of NONE=>SOME([s],ts)
        | SOME ts=>
        (case parse_inputs ts
          of NONE=>NONE
          | SOME (args,ts)=> SOME(s::args,ts))))
  end


