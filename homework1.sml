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

fun sum [] = 0
  | sum (x::xs')= x+(sum xs')

fun prod [] = 1
  | prod (x::xs)= x*(prod xs)

fun every_other_help [] _= []
  | every_other_help (x::xs) true= x::(every_other_help xs false)
  | every_other_help (x::xs) false=(every_other_help xs true ) 

fun every_other xs = 
  every_other_help xs true

fun flatten [] = []
  | flatten (xs::xss)= xs@(flatten xss) 

fun heads [] = []
  | heads ([]::xss)= (heads xss)
  | heads (xs::xss)=(hd xs)::(heads xss)

fun tails [] = []
  | tails ([]::xss)=(tails xss)
  | tails (xs::xss)=(tl xs)::(tails xss)

fun scaleMatHelp m []=[]
  | scaleMatHelp m (x::xs)=(m*x)::(scaleMatHelp m xs)

fun scaleMat m []=[]
  | scaleMat m ([]::xss) =(scaleMat m xss)
  | scaleMat m (xs::xss) =(scaleMatHelp m xs)::(scaleMat m xss)

fun curl ix len work total []=total@[work]
  | curl ix len work total (x::xs)= 
      if ix<len
      then (curl (ix+1) len (work@[x]) total xs)
      else (curl 0 len [] (total@[work]) (x::xs))  

fun addMatHelp [] []= []
  | addMatHelp [] _ = raise Fail "my god what have you done (malformed matrices)" 
  | addMatHelp _ [] = raise Fail "my god what have you done (malformed matrices)"
  | addMatHelp (x::xs) (y::ys) = y+x::(addMatHelp xs ys)

fun addMat m1 m2=
  (curl 0 (length (hd m1)) [] [] (addMatHelp (flatten m1) (flatten m2)))

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

(*fun reduce_help (n,d) cur high stop=
  if (cur>stop)
    then high
    else
      if (((n mod cur)=0) andalso ((d mod cur)=0)) 
      then reduce_help (n,d) (cur+1) cur stop 
      else (reduce_help (n,d) (cur+1) high stop);;

fun reduce r=
  let val high=reduce_help r 1 1 (min n d) in 
  ((n/high),(d/high)) end*)


fun simplifyRat (n,d) =
  let val gcdVar= (gcd n d) in
  let val n'=round ((real n)/(real gcdVar)) in 
  let val d'=round ((real d)/(real gcdVar)) in
  if (d'=0)
  then raise DivisionByZero "Oh the humanity! (Div by zero)"
  else if (d'=1)
       then VInt (n')
       else VRat (n',d')
  end
  end
  end

fun makeRat i= (i,1)

fun invRat (n1,d1)=(d1,n1)

fun addRat (n1,d1) (n2,d2) =
  let val lcmVar= lcm d1 d2 in
  let val d1coef= lcmVar div d1 in
  let val d2coef= lcmVar div d2 in
  let val d1'= d1*d1coef in
  let val d2'= d2*d2coef in
  let val n1'= n1*d1coef in
  let val n2'= n2*d2coef in
(*  ((n1',d1'),(n2',d2'))*)
  let val n3=n1'+n2' in
  let val d3=d1' in
  simplifyRat (n3,d3)
  end end end end end end end end end
  (*^^This is the best line of code I have written...ever*)

fun mulRat (n1,d1) (n2,d2) = 
  let val n3=n1*n2 in
  let val d3=d1*d2 in
  simplifyRat(n3,d3)
  end end

fun negRat (n,d) = 
  simplifyRat(~n,d)

fun applyAdd (VInt i) (VInt j) = VInt (i+j)
  | applyAdd (VVec v) (VVec w) = VVec (addVec v w)
  | applyAdd (VMat m) (VMat n) = VMat (addMat n m)
  | applyAdd (VRat r) (VInt i) = addRat (makeRat i) r
  | applyAdd (VInt i) (VRat r) = addRat (makeRat i) r
  | applyAdd (VRat r1) (VRat r2)= addRat r1 r2
  | applyAdd _ _ = raise TypeError "applyAdd"

fun applyMul (VInt i) (VInt j) = VInt (i*j)
  | applyMul (VInt i) (VVec v) = VVec (scaleVec i v)
  | applyMul (VVec v) (VVec w) = VInt (inner v w)
  | applyMul (VInt m) (VMat n) = VMat (scaleMat m n)
  | applyMul (VMat n) (VInt m) = VMat (scaleMat m n)
  | applyMul (VInt i) (VRat r) = mulRat (makeRat i) r
  | applyMul (VRat r) (VInt i) = mulRat (makeRat i) r
  | applyMul (VRat r1) (VRat r2) = mulRat r1 r2
  | applyMul _ _ = raise TypeError "applyMul"

(*Not sure why, but the compiler yelled at me about redundant match
  on the last one here so I had to comment out.*)
fun applyNeg (VInt i) = VInt (~ i)
  | applyNeg (VVec v) = VVec (scaleVec ~1 v)
  | applyNeg (VMat m) = VMat (scaleMat ~1 m)
  | applyNeg (VRat r) = (negRat r)
 (* | applyNeg _ = raise TypeError "applyNeg"*)

fun applySub a b = applyAdd a (applyNeg b)

fun applyDiv (VInt i) (VRat r)= mulRat (makeRat i) (invRat r)
  | applyDiv (VRat r) (VInt i)= mulRat r (invRat (makeRat i))
  | applyDiv (VInt i1) (VInt i2)=mulRat (makeRat i1) (invRat (makeRat i2))
  | applyDiv (VRat r1) (VRat r2)=mulRat r1 (invRat r2)
  | applyDiv _ _ = raise TypeError "applyDiv"

fun eval (EInt i) = VInt i
  | eval (EAdd (e,f)) = applyAdd (eval e) (eval f)
  | eval (ESub (e,f)) = applySub (eval e) (eval f)
  | eval (EMul (e,f)) = applyMul (eval e) (eval f)
  | eval (ENeg e) = applyNeg (eval e)
  | eval (EVec v) = VVec v
  | eval (EMat m) = VMat m
  | eval (EDiv (e,f)) = applyDiv (eval e) (eval f)
