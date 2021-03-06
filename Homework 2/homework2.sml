

(*
 * Code for HOMEWORK 2
 *
 *)



(* Internal representation *)

datatype value = VInt of int
               | VBool of bool
               | VList of value list
      	       | VPair of value * value
               | VFun of function

and expr = EVal of value
         | EAdd of expr * expr
         | ESub of expr * expr
         | EMul of expr * expr
         | ENeg of expr
         | EEq of expr * expr
         | EIf of expr * expr * expr
         | ELet of string * expr * expr
         | EIdent of string
         | ECall of string * expr

         | ECons of expr * expr
         | EIsEmpty of expr
         | EHead of expr
         | ETail of expr

         | EPair of expr * expr
         | EFirst of expr
         | ESecond of expr

         | ESlet of (string * expr) list * expr

         | ECallE of expr * expr

and function = FDef of string * expr   



(* Functions to create errors *)

fun evalError msg = raise Fail ("Eval Error @ "^msg)

fun unimplemented msg = raise Fail ("CODE NOT IMPLEMENTED - "^msg)




(* Primitive operations *)

fun applyAdd (VInt i1) (VInt i2) = VInt (i1+i2)
  | applyAdd _ _ = evalError "applyAdd"

fun applyMul (VInt i1) (VInt i2) = VInt (i1*i2)
  | applyMul _ _ = evalError "applyMul"

fun applyNeg (VInt i) = VInt (~ i)
  | applyNeg _ = evalError "applyNeg"

fun applyEq (VInt i1) (VInt i2) = VBool (i1 = i2)
  | applyEq (VBool b1) (VBool b2) = VBool (b1 = b2)
  | applyEq _ _ = evalError "applyEq"

fun applySub v1 v2 = applyAdd v1 (applyNeg v2)




(* COMPLETE THE FOLLOWING FOR QUESTION 1 *)

fun applyPair x y= VPair (x,y)

fun applyFirst (VPair (x,y))= x
  | applyFirst _= evalError "I have lived as few have dared to dream...(argument is not a pair)"

fun applySecond (VPair (x,y))=y
  | applySecond _=evalError "This is the last of Earth, I am content...(argument is not a pair)"



(* COMPLETE THE FOLLOWING FOR QUESTION 3 *)

fun applyCons head (VList tail) = 
   (VList (head::tail))
  | applyCons _ _=evalError "Weep for me not, for my glory shall be eternal...(Type mismatch in Cons)"

fun applyIsEmpty (VList inputList) = 
  (VBool (List.null inputList))
  | applyIsEmpty _ = (VBool false)

fun applyHead (VList (hd::tl))=hd 
  | applyHead _ = evalError "There is nothing proper about what you are doing, soldier, but do try to kill me properly...(Type mismatch in IsEmpty)"

fun applyTail (VList (hd::tl)) =(VList tl)
  | applyTail _= evalError "It's better to burn out than to fade away...(Type Error in applyTail)"






(* Substitution function -- COMPLETE the missing cases *)
(*e what youre going to substitue in (X*X)*)
(*id is what youre going to substitute for (x)*)
(*v is the value (10)*)

fun getBinders []= []
  | getBinders ((varName,expression)::rest)=
    varName::(getBinders rest) 


(*substitute *)
fun subst (EVal v) id e = EVal v
  | subst (EAdd (e1,e2)) id e = EAdd (subst e1 id e, subst e2 id e)
  | subst (ESub (e1,e2)) id e = ESub (subst e1 id e, subst e2 id e)
  | subst (EMul (e1,e2)) id e = EMul (subst e1 id e, subst e2 id e)
  | subst (ENeg e1) id e = ENeg (subst e1 id e)
  | subst (EEq (e1,e2)) id e = EEq (subst e1 id e, subst e2 id e)
  | subst (EIf (e1,e2,e3)) id e = EIf (subst e1 id e, 
                                       subst e2 id e,
                                       subst e3 id e)
  | subst (ELet (id',e1,e2)) id e = 
      if id = id'
      then ELet (id',subst e1 id e, e2)
      else ELet (id',subst e1 id e, subst e2 id e)
  | subst (EIdent id') id e = if id = id'
                                then e
                              else EIdent id'
  | subst (ECall (n,e1)) id e = ECall (n,subst e1 id e)
  | subst (ECons (e1,e2)) id e = ECons (subst e1 id e,subst e2 id e)
  | subst (EIsEmpty e1) id e = EIsEmpty (subst e1 id e)
  | subst (EHead e1) id e = EHead (subst e1 id e)
  | subst (ETail e1) id e = ETail (subst e1 id e)
  | subst (EPair (e1,e2)) id e = EPair (subst e1 id e,subst e2 id e)
  | subst (EFirst e1) id e = EFirst (subst e1 id e)
  | subst (ESecond e1) id e = ESecond (subst e1 id e)  
  | subst (ESlet (bnds,e1)) id e=
    
    if (List.exists (fn x=>x=id) (getBinders bnds))
    then ESlet((substList id e bnds), e1)
    else ESlet((substList id e bnds), (subst e1 id e))


  | subst (ECallE (e1,e2)) id e = ECallE ((subst e1 id e), (subst e2 id e))

and substList id e []=[]
  | substList id e ((id',e')::rest)=
    (id',(subst e' id e))::(substList id e rest)


(* Lookup a function name in the function environment *)
fun lookup name [] = evalError ("Now I shall go to sleep. Goodnight...(lookup - "^name^")")
  | lookup name ((n,f)::fenv) = 
      if (n = name)
        then f
      else lookup name fenv 



(* Evaluation function -- COMPLETE the missing cases *)

fun eval _ (EVal v) = v
  | eval fenv (EAdd (e1,e2)) = applyAdd (eval fenv e1) (eval fenv e2)
  | eval fenv (ESub (e1,e2)) = applySub (eval fenv e1) (eval fenv e2)
  | eval fenv (EMul (e1,e2)) = applyMul (eval fenv e1) (eval fenv e2)
  | eval fenv (ENeg e) = applyNeg (eval fenv e)
  | eval fenv (EEq (e1,e2)) = applyEq (eval fenv e1) (eval fenv e2)
  | eval fenv (EIf (e1,e2,e3)) = evalIf fenv (eval fenv e1) e2 e3
  | eval fenv (ELet (n,e1,e2)) = evalLet fenv n (eval fenv e1) e2
  | eval fenv (EIdent id) = applyIdent id fenv
  | eval fenv (ECall (name,e)) = evalCall fenv (lookup name fenv) (eval fenv e)
  | eval fenv (ESlet (bnds,e)) = (evalSLet fenv (evalBindings fenv bnds) e)
  | eval fenv (ECons (e1,e2)) = applyCons (eval fenv e1) (eval fenv e2)
  | eval fenv (EIsEmpty e) = applyIsEmpty (eval fenv e)
  | eval fenv (EHead e) = applyHead (eval fenv e)
  | eval fenv (ETail e) = applyTail (eval fenv e)
  | eval fenv (EPair (e1,e2)) = applyPair (eval fenv e1) (eval fenv e2)
  | eval fenv (EFirst e) = applyFirst (eval fenv e) 
  | eval fenv (ESecond e) = applySecond (eval fenv e)
  | eval fenv (ECallE (func, e)) = evalCallE (eval fenv func) (eval fenv e) fenv

and applyIdent id fenv=
  (VFun (lookup id fenv))

and evalCallE (VFun (FDef (param,body))) e fenv=
    eval fenv (subst body param (EVal e))

and evalCall fenv (FDef (param,body)) arg = 
      eval fenv (subst body param (EVal arg))

and evalIf fenv (VBool true) ethen eelse = eval fenv ethen
  | evalIf fenv (VBool false) ethen eelse = eval fenv eelse
  | evalIf _ _ _ _ = evalError "evalIf"

and evalLet fenv id v body = eval fenv (subst body id (EVal v))

and evalBindings fenv [] = []
  | evalBindings fenv ((a,b)::tl)=
    (a,(EVal (eval fenv b)))::(evalBindings fenv tl)

and evalSLet fenv [] body= (eval fenv body)
  | evalSLet fenv ((name,localbdy)::tl) body=
    evalSLet fenv tl (subst body name localbdy)



(* Sample functions for testing *)

val succ = ("succ",
            FDef ("n",
	          EAdd (EIdent "n", EVal (VInt 1))))

val pred = ("pred",
	    FDef ("n",
		  ESub (EIdent "n", EVal (VInt 1))))

val exp = ("exp",
    	   FDef ("args",
		 ELet ("a", EFirst (EIdent "args"),
		       ELet ("n", ESecond (EIdent "args"),
			     EIf (EEq (EIdent "n", EVal (VInt 0)),
				  EVal (VInt 1),
				  EMul (EIdent "a",
					ECall ("exp",
					       EPair (EIdent "a",
						      ECall ("pred",
							     EIdent "n")))))))))

val addT = ("addT", 
	    FDef ("args",
		  ELet ("a", EFirst (EIdent "args"),
			ELet ("b", ESecond (EIdent "args"),
			      EAdd (EIdent "a", EIdent "b")))))

val swap = ("swap",
	    FDef ("args",
		  ELet ("x", EFirst (EIdent "args"),
			ELet ("y", ESecond (EIdent "args"),
			      ESlet ([("x",EIdent "y"),("y",EIdent "x")],
				     EPair (EIdent "x", 
					    EIdent "y"))))))

val append = ("append", 
	      FDef ("args",
		    ELet ("xs",EFirst (EIdent "args"),
			  ELet ("ys",ESecond (EIdent "args"),
				EIf (EIsEmpty (EIdent "xs"),
				     EIdent "ys",
				     ECons (EHead (EIdent "xs"),
					    ECall ("append",
						   EPair (ETail (EIdent "xs"),
							  EIdent "ys"))))))))

val length = ("length",
	      FDef ("xs",
		    EIf (EIsEmpty (EIdent "xs"),
			 EVal (VInt 0),
			 EAdd (EVal (VInt 1),
			       ECall ("length", ETail (EIdent "xs"))))))



val twice = ("twice",
             FDef ("args",
		   ELet ("f", EFirst (EIdent "args"),
			 ELet ("x", ESecond (EIdent "args"),
			       ECallE (EIdent "f", 
				       ECallE (EIdent "f",
					       EIdent "x"))))))

val mapf = ("mapf",
	    FDef ("args",
		  ELet ("f", EFirst (EIdent "args"),
			ELet ("xs", ESecond (EIdent "args"),
			      EIf (EIsEmpty (EIdent "xs"),
				   EVal (VList []),
				   ECons (ECallE (EIdent "f",
						  EHead (EIdent "xs")),
					  ECall ("mapf",
						 EPair (EIdent "f",
							ETail (EIdent "xs")))))))))
