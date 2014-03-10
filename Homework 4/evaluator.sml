(* 
 *   CODE FOR HOMEWORK 4
 *)


structure Evaluator = struct

  structure I = InternalRepresentation



  exception Evaluation of string

  fun evalError msg = raise Evaluation msg


  (* 
   *   Primitive operations
   *)

  fun primPlus (I.VInt a) (I.VInt b) = I.VInt (a+b)
    | primPlus _ _ = evalError "primPlus"

  fun primMinus (I.VInt a) (I.VInt b) = I.VInt (a-b)
    | primMinus _ _ = evalError "primMinus"

  fun primTl (I.VList a) = I.VList (List.tl(a))
    | primTl _ = evalError "primHd - not a list"

(*  fun emptyList() =
     primTl (I.VList ((I.VInt 1)::[]))*)

  fun primEq (I.VInt a) (I.VInt b) = I.VBool (a=b)
    | primEq (I.VBool a) (I.VBool b) = I.VBool (a=b)
    | primEq (I.VList []) (I.VList[])= I.VBool true
    | primEq (I.VList _) (I.VList[])= I.VBool false
    | primEq (I.VList []) (I.VList _)= I.VBool false
    | primEq (I.VList a) (I.VList b) = 
      (case (primEq (List.hd a) (List.hd b))
        of (I.VBool true)=> primEq (I.VList (List.tl a)) (I.VList (List.tl b)) 
        | (I.VBool false)=> I.VBool false
        | _ => evalError "whaaaaa?")
    | primEq _ _ = I.VBool false

  fun primLess (I.VInt a) (I.VInt b) = I.VBool (a<b)
    | primLess _ _ = I.VBool false

  fun primCons (I.VInt a) (I.VList b) = I.VList ((I.VInt a)::b)
    |primCons  (I.VBool a) (I.VList b) = I.VList ((I.VBool a)::b)
    |primCons  _ _ = evalError "Incorrect Usage of Cons"

  fun primHd (I.VList a) = List.hd(a)
    | primHd _ = evalError "primHd - not a list"

  fun primInterval (I.VInt i) (I.VInt j)=
    if (j<i) 
      then (I.VList []) 
      else (primCons (I.VInt i) (primInterval (I.VInt (i+1)) (I.VInt j)))
    | primInterval a _= evalError "Type error in primInterval"




(*  fun primMap (I.EFun f) (I.VList []) = I.VList []
    | primMap (I.EFun (name,ex)) (I.VList xs)=
    let val expr= (I.EApp ((I.EFun (name,ex)),(I.EVal (primHd(I.VList xs))))) in
    let val value= I.VClosure(name,ex,[]) in 
      (primCons value (primMap (I.EFun (name,ex)) primTl(I.VList xs)))
    end
    end*)

			 
  fun lookup (name:string) [] = evalError ("failed lookup for "^name)
    | lookup name ((n,v)::env) = 
        if (n = name) then 
	  v
	else lookup name env 

  fun extractList (I.VRecord ts)=ts
    | extractList _ =evalError "Not a Record"



  (*
   *   Evaluation functions
   * 
   *)


  fun eval _ (I.EVal v) = v
    | eval env (I.EFun (n,e)) = I.VClosure (n,e,env)
    | eval env (I.EIf (e,f,g)) = evalIf env (eval env e) f g
    | eval env (I.ELet (name,e,f)) = evalLet env name (eval env e) f
    | eval env (I.ELetFun (name,param,e,f)) = evalLetFun env name param e f
    | eval env (I.EIdent n) = lookup n env
    | eval env (I.EApp (e1,e2)) = evalApp env (eval env e1) (eval env e2)
    | eval env (I.EPrimCall1 (f,e1)) = f (eval env e1)
    | eval env (I.EPrimCall2 (f,e1,e2)) = f (eval env e1) (eval env e2)
    | eval env (I.ERecord fs) = I.VRecord (evalRecord env fs)
    | eval env (I.EField (e,s)) = lookup s (extractList (eval env e))
      
  and evalApp _ (I.VClosure (n,body,env)) v = eval ((n,v)::env) body
    | evalApp _ (I.VRecClosure (f,n,body,env)) v = let
	  val new_env = [(f,I.VRecClosure (f,n,body,env)),(n,v)]@env
      in 
	  eval new_env body
      end
    | evalApp _ _ _ = evalError "cannot apply non-functional value"

  and evalIf env (I.VBool true) f g = eval env f
    | evalIf env (I.VBool false) f g = eval env g
    | evalIf _ _ _ _ = evalError "evalIf"
		       
  and evalLet env id v body = eval ((id,v)::env) body

  and evalLetFun env id param expr body = let
      val f = I.VRecClosure (id, param, expr, env)
  in
      eval ((id,f)::env) body
  end 

  and evalRecord env [] = []
    | evalRecord env ((n,e)::xs) = (n,eval env e)::evalRecord env xs


  fun primMap (I.VClosure f) (I.VList []) = I.VList []
    | primMap (I.VClosure f) (I.VList xs) = 
        primCons (eval [] (I.EApp ((I.EVal (I.VClosure f)),(I.EVal (primHd(I.VList xs)))))) (primMap (I.VClosure f) (primTl (I.VList xs)))
    | primMap _ _= evalError "Type error in primMap"


  fun primFilter (I.VClosure f) (I.VList []) = I.VList [] 
    | primFilter (I.VClosure f) (I.VList xs)= 
      (eval [] (I.EIf(I.EApp (I.EVal (I.VClosure f),I.EVal (primHd (I.VList xs))),
        I.EVal (primCons (primHd (I.VList xs)) (primFilter (I.VClosure f) (primTl (I.VList xs)))),
        I.EVal (primFilter (I.VClosure f) (primTl (I.VList xs))))))
    | primFilter _ _= evalError "Type error in primFilter"

  (* 
   *   Initial environment (already in a form suitable for the environment)
   *)

  val initialEnv = 
      [("add", I.VClosure ("a", 
			   I.EFun ("b", 
				   I.EPrimCall2 (primPlus,
						 I.EIdent "a",
						 I.EIdent "b")),
			   [])),
       ("sub", I.VClosure ("a", 
			   I.EFun ("b", 
				   I.EPrimCall2 (primMinus,
						 I.EIdent "a",
						 I.EIdent "b")),
			   [])),
       ("equal", I.VClosure ("a",
			  I.EFun ("b",
				  I.EPrimCall2 (primEq,
						I.EIdent "a",
						I.EIdent "b")),
			  [])),
       ("less", I.VClosure ("a",
			    I.EFun ("b",
				    I.EPrimCall2 (primLess,
						  I.EIdent "a",
						  I.EIdent "b")),
			    [])),
        ("nil",I.VList []),

       ("hd", I.VClosure ("a",
          I.EPrimCall1 (primHd, I.EIdent "a"),
          [])),

        ("tl", I.VClosure ("a",
          I.EPrimCall1 (primTl, I.EIdent "a"),
          [])),

        ("interval",I.VClosure("a",
          I.EFun("b",
            I.EPrimCall2 (primInterval,
              I.EIdent "a",
              I.EIdent "b")),
          [])),

        ("map",I.VClosure("f",
          I.EFun("xs",
            I.EPrimCall2(primMap,
              I.EIdent "f",
              I.EIdent "xs")),
          [])),

        ("filter",I.VClosure("f",
          I.EFun("xs",
            I.EPrimCall2(primFilter,
              I.EIdent "f",
              I.EIdent "xs")),
          [])),

        ("cons", I.VClosure ("a",
          I.EFun ("b",
            I.EPrimCall2 (primCons,
              I.EIdent "a",
              I.EIdent "b")),
          []))]
  
				 
end
