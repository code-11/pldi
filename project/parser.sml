 
 (*   CODE FOR HOMEWORK 5
 *)



structure Parser =  struct

  (*
   *  Wrapper around the regexp library
   *)      

  structure R = RegExpFn (structure P = AwkSyntax structure E = DfaEngine)

(*  structure I = InternalRepresentation
*)                
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
                 | T_TRUE
                 | T_FALSE
                 | T_IF
		             | T_THEN
                 | T_ELSE
                 | T_EQUAL
                 | T_LPAREN 
                 | T_RPAREN
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

  fun stringOfToken (T_SYM s) = "T_SYM["^s^"]"
    | stringOfToken (T_INT i) = "T_INT["^(Int.toString i)^"]"
    | stringOfToken (T_STRING s) = "T_STRING["^s^"]"
    | stringOfToken T_TRUE = "T_TRUE"
    | stringOfToken T_FALSE = "T_FALSE"
    | stringOfToken T_IF  = "T_IF"
    | stringOfToken T_THEN  = "T_THEN"
    | stringOfToken T_ELSE  = "T_ELSE"
    | stringOfToken T_EQUAL = "T_EQUAL"
    | stringOfToken T_LPAREN = "T_LPAREN"
    | stringOfToken T_RPAREN = "T_RPAREN"
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

                   
  fun whitespace _ = NONE
                     
  fun produceSymbol "true" = SOME (T_TRUE)
    | produceSymbol "false" = SOME (T_FALSE)
    | produceSymbol "if" = SOME (T_IF)
    | produceSymbol "then" = SOME (T_THEN)
    | produceSymbol "else" = SOME (T_ELSE)
    | produceSymbol "while" = SOME (T_WHILE)
    | produceSymbol "var" = SOME (T_VAR)
    | produceSymbol "for" = SOME (T_FOR)
    | produceSymbol "class" = SOME (T_CLASS)
    | produceSymbol "continue" = SOME (T_CONTINUE)
    | produceSymbol "instanceof" = SOME (T_INSOF)
    | produceSymbol "new" = SOME (T_NEW)
    | produceSymbol "return" = SOME (T_RETURN)
    | produceSymbol "super" = SOME (T_SUPER)
    | produceSymbol "this" = SOME (T_THIS)
    | produceSymbol "null" = SOME (T_NULL)
    | produceSymbol text = SOME (T_SYM text)

  fun produceString text = SOME (T_STRING text)
                           
  fun produceInt text = (case Int.fromString text
                          of NONE => parseError "integer literal out of bounds"
                           | SOME i => SOME (T_INT i))
                        
  fun produceEqual _ = SOME (T_EQUAL)
  fun produceLParen _ = SOME (T_LPAREN)
  fun produceRParen _ = SOME (T_RPAREN)

  fun produceLBrace _ = SOME (T_LBRACE)
  fun produceRBrace  _ = SOME (T_RBRACE)

  fun producePlus _ = SOME (T_PLUS)
  fun produceTimes _ = SOME (T_TIMES)
  fun produceComma _ = SOME (T_COMMA)
  fun produceAssign _ =SOME (T_ASSIGN)

  fun produceSemiColon _ = SOME (T_SEMICOLON)
                       
  val tokens = let 
    fun convert (re,f) = (R.compileString re, f)
  in
    map convert [("( |\\n|\\t)+",         whitespace),
                 ("==",                   produceEqual),
                 ("\\+",                  producePlus),
            		 ("\\*",                  produceTimes),
            		 (",",                    produceComma),
            		 (";",                    produceSemiColon),
                 ("[a-zA-Z][a-zA-Z0-9]*", produceSymbol),
                 ("~?[0-9]+",             produceInt),
                 ("\\(",                  produceLParen),
                 ("\\)",                  produceRParen),
                 ("{",                    produceLBrace),
                 ("}",                    produceRBrace),
                 ("=",                    produceAssign),
                 ("\\"[^\\"]*\\"",   produceString)]
  end
               
               
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
end  


