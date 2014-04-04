structure Compiler = struct

  structure I = InternalRepresentation
  structure S = StackRepresentation


  exception Compilation of string

  fun compileError msg = raise Compilation msg



  (* 
   *   helper functions for working with sentences
   * 
   *  appendS : sentence -> sentence -> sentence // appends two sentences
   *  appendSs : sentence list -> sentence       // appends list of sentences
   *  wordS : word -> sentence                   // sentence with only a word
   *)
  
  fun appendS (S.SEmpty) s = s
    | appendS (S.SSequence (w,s1)) s2 = S.SSequence (w,appendS s1 s2)
    | appendS (S.SIf (s1,s2,s3)) s4 = S.SIf (s1,s2,appendS s3 s4)
    | appendS (S.SWhile (s1,s2)) s3 = S.SWhile (s1,appendS s2 s3)

  fun appendSs [] = S.SEmpty
    | appendSs (s1::ss) = appendS s1 (appendSs ss)

  fun wordS (w) = S.SSequence (w,S.SEmpty)



  (* TO COMPLETE *)

  fun compileV (I.VInt i) = wordS i
    | compileV (I.VBool b) = wordS
    | compileV (I.VList l) = compileError "compileV/VList"


  and compileE (I.EVal v) = compileError "compileE/EVal"
    | compileE (I.EAdd (e1,e2)) = compileError "compileE/EAdd"
    | compileE (I.ESub (e1,e2)) = compileError "compileE/ESub"
    | compileE (I.EMul (e1,e2)) = compileError "compileE/EMul"
    | compileE (I.EEq (e1,e2)) = compileError "compileE/EEq"
    | compileE (I.ECons (e1,e2)) = compileError "compileE/ECons"
    | compileE (I.EHead e1) = compileError "compileE/EHead"
    | compileE (I.ETail e1) = compileError "compileE/ETail"
    | compileE (I.EIf (e1,e2,e3)) = compileError "compileE/EIf"

    | compileE (I.ELet (name,e1,e2)) = compileError "compileE/ELet"
    | compileE (I.EIdent name) = compileError "compileE/EIdent"

    | compileE (I.ECall (name,es)) = compileError "compileE/ECall"


  fun compileExpr expr = let
      val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])
      val sent = compileE expr
      val _ = print (String.concat ["[  ", S.stringOfSentence sent, " ]\n"])
  in
      sent
  end



  (* TO COMPLETE -- a definition should return a sentence associated with
   *  the name being defined. Calling the name from the stack language
   *  with the arguments on the stack in the right order should execute
   *  the body of the compiled function *)

  fun compileDef name params expr = let
    val _ = print (String.concat ["[compiling ", I.stringOfExpr expr, "]\n"])
    val sent = compileError "cannot compile definitions yet"
    val _ = print (String.concat ["[  ", S.stringOfSentence sent, " ]\n"])
  in
    sent
  end



end
