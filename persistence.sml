(* 2. Persistence *)

(* 2.1 Lists *)

signature STACK =
sig
  type 'a Stack
  val empty : 'a Stack
  val isEmpty : 'a Stack -> bool

  val cons : 'a * 'a Stack -> 'a Stack
  val head : 'a Stack -> 'a
  val tail : 'a Stack -> 'a Stack
end

(* Implementation of stacks using the built-in type of lists *)

structure List : STACK =
struct
  type 'a Stack = 'a list

  val empty = []
  fun isEmpty s = null s

  fun cons(x,s) = x :: s
  fun head s = hd s
  fun tail s = tl s
end

(* Implementation of stacks using a custom datatype *)

structure CustomStack : STACK =
struct
  datatype 'a Stack = NIL | CONS of 'a * 'a Stack

  exception EMPTY (* I added this *)

  val empty = NIL
  fun isEmpty NIL = true | isEmpty _ = false

  fun cons(x,s) = CONS(x,s)
  fun head NIL = raise EMPTY
    | head (CONS(x,s)) = x
  fun tail NIL = raise EMPTY
    | tail (CONS(x,s)) = s
end

(* I wonder how this deals with consing different types? *)

(*

- CustomStack.cons("a",CustomStack.cons(1,CustomStack.empty));
stdIn:2.1-2.60 Error: operator and operand don't agree [overload conflict]
operator domain: string * string CustomStack.Stack
operand:         string * [int ty] CustomStack.Stack
in expression:
CustomStack.cons ("a",CustomStack.cons (1,CustomStack.empty))

*)

infixr ++

fun xs ++ ys =
  if CustomStack.isEmpty xs 
  then ys
  else CustomStack.cons (CustomStack.head xs,CustomStack.tail xs ++ ys);

(* val ++ = fn :  'a CustomStack.Stack * 'a CustomStack.Stack -> 'a CustomStack.Stack *)

(* Interesting, the compiler emits a warning if you try to concatenate two empty stacks *)

(*

- CustomStack.empty ++ CustomStack.empty
= ;
stdIn:4.1-4.39 Warning: type vars not generalized because of
   value restriction are instantiated to dummy types (X1,X2,...)
val it = NIL : ?.X1 CustomStack.Stack

*)

(* If we have access to the underlying representation, then ++ can be rewritten using pattern matching *)

(*
fun [] ++ ys = ys
  | (x::xs) ++ ys = x :: (xs ++ ys);

*)

(* val ++ = fn : 'a list * 'a list -> 'a list *)

exception SUBSCRIPT;

fun update([],i,y) = raise SUBSCRIPT
  | update(x::xs,0,y) = y :: xs
  | update(x::xs,i,y) = x :: update(xs,i-1,y);

(* val update = fn : 'a list * int * 'a -> 'a list *)

update(List.cons(1,List.cons(2,List.cons(3,List.empty))),1,4);

(* val it = [1,4,3] : int list *)

(* Exercise 2.1 *)

fun suffixes [] = [[]]
  | suffixes x = x :: suffixes (tl x);

suffixes [1,2,3,4];

(* val it = [[1,2,3,4],[2,3,4],[3,4],[4],[]] : int list list *)

