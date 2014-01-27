(*
 * PLDI (Spring 2014)
 *
 * Code for HOMEWORK 1
 *
 *)



fun scaleVec a [] = []
  | scaleVec a (x::xs) = (a*x)::(scaleVec a xs)

fun addVec [] [] = []
  | addVec (x::xs) (y::ys) = (x+y)::(addVec xs ys)
  | addVec _ _ = []

fun inner [] [] = 0
  | inner (x::xs) (y::ys) = (x*y) + (inner xs ys)
  | inner _ _ = 0



(* Question 1 *)


fun gcd a b = 
  if 
    b=0
  then
    a
  else
    gcd b (a mod b)

fun lcm a b = 
  let val c=(a*b) in
  let val temp=(gcd a b) in
  abs(c div temp)
  end
  end 


fun exp a n = 
  if 
    n<=0 
  then
    1
  else a*(exp a (n-1))

fun tetra a n = 
  if 
    n<=0
  then 
    1
  else 
   (exp a (tetra a (n-1)))



(* Question 2 *)

fun sum xs = raise Fail "sum not implemented"

fun prod xs = raise Fail "prod not implemented"

fun every_other xs = raise Fail "every_other not implemented"

fun flatten xss = raise Fail "flatten not implemented"

fun heads xss = raise Fail "heads not implemented"

fun tails xss = raise Fail "tails not implemented"

fun scaleMat a m = raise Fail "scaleMat not implemented"

fun addMat m1 m2 = raise Fail "addMat not implemented"



(* QUESTIONS 3 & 4 *)

exception TypeError of string

exception DivisionByZero of string

datatype value = VInt of int
	       | VVec of int list
	       | VMat of int list list
	       | VRat of int * int

datatype expr = EInt of int
	      | EVec of int list
	      | EMat of int list list
	      | EAdd of expr * expr
	      | ESub of expr * expr
	      | EMul of expr * expr
	      | ENeg of expr
	      | EDiv of expr * expr

fun simplifyRat r = raise Fail "simplifyRat not implemented"

fun addRat r s = raise Fail "addRat not implemented"

fun mulRat r s = raise Fail "mulRat not implemented"

fun negRat r = raise Fail "negRat not implemented"

fun applyAdd (VInt i) (VInt j) = VInt (i+j)
  | applyAdd (VVec v) (VVec w) = VVec (addVec v w)
  | applyAdd _ _ = raise TypeError "applyAdd"

fun applyMul (VInt i) (VInt j) = VInt (i*j)
  | applyMul (VInt i) (VVec v) = VVec (scaleVec i v)
  | applyMul (VVec v) (VVec w) = VInt (inner v w)
  | applyMul _ _ = raise TypeError "applyMul"

fun applyNeg (VInt i) = VInt (~ i)
  | applyNeg (VVec v) = VVec (scaleVec ~1 v)
  | applyNeg _ = raise TypeError "applyNeg"

fun applySub a b = applyAdd a (applyNeg b)


fun eval (EInt i) = VInt i
  | eval (EAdd (e,f)) = applyAdd (eval e) (eval f)
  | eval (ESub (e,f)) = applySub (eval e) (eval f)
  | eval (EMul (e,f)) = applyMul (eval e) (eval f)
  | eval (ENeg e) = applyNeg (eval e)
  | eval (EVec v) = VVec v
  | eval (EMat m) = raise Fail "eval/EMat not implemented"
  | eval (EDiv (e,f)) = raise Fail "eval/EDiv not implemented"
