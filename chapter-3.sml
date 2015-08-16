(* 3. Some Familiar Data Structures in a Functional Setting *)

(* bringing in some things from the previous chapter.. *)

signature ORDERED =
sig
  type T

  val eq : T * T -> bool
  val lt : T * T -> bool
  val leq : T * T -> bool
end

(* 3.1 Leftist Heaps *)

signature HEAP =
sig
  structure Elem: ORDERED

  type Heap

  val empty : Heap
  val isEmpty : Heap -> bool

  val insert : Elem.T * Heap -> Heap
  val merge : Heap * Heap -> Heap

  val findMin : Heap -> Elem.T
  val deleteMin : Heap -> Heap
end;

exception EMPTY;

functor LeftistHeap(Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Heap = E | T of int * Elem.T * Heap * Heap

  fun rank E = 0
    | rank (T(r,_,_,_)) = r
  
  fun makeT(x,a,b) = if rank a >= rank b then T(rank b + 1,x,a,b)
                     else T(rank a + 1,x,b,a)

  val empty = E
  fun isEmpty E = true | isEmpty _ = false

  fun merge(h,E) = h
    | merge(E,h) = h
    | merge(h1 as T(_,x,a1,b1), h2 as T(_,y,a2,b2)) =
        if Elem.leq(x,y) then makeT(x,a1,merge(b1,h2))
        else makeT(y,a2,merge(h1,b2))

  fun insert(x,h) = merge(T(1,x,E,E),h)

  fun findMin E = raise EMPTY
    | findMin (T(_,x,a,b)) = x

  fun deleteMin E = raise EMPTY
    | deleteMin (T(_,x,a,b)) = merge(a,b)

end;

(* 3.2 Binomial Heaps *)

functor BinomialHeap(Element : ORDERED) : HEAP =
struct
  structure Elem = Element

  datatype Tree = Node of int * Elem.T * Tree list
  type Heap = Tree list

  val empty = []
  fun isEmpty ts = null ts
    
  fun rank(Node(r,x,c)) = r
  fun root(Node(r,x,c)) = x
  fun link(t1 as Node(r,x1,c1), t2 as Node(_,x2,c2)) =
    if Elem.leq(x1,x2) then Node(r + 1,x1,t2::c1)
    else Node(r+1,x2,t1::c2)
  fun insTree(t,[]) = [t]
    | insTree(t,ts as t'::ts') =
        if rank t < rank t' then t::ts else insTree(link(t,t'),ts')

  fun insert(x,ts) = insTree(Node(0,x,[]),ts)

  fun merge(ts1,[]) = ts1
    | merge([],ts2) = ts2
    | merge(ts1 as t1::ts1',ts2 as t2::ts2') =
        if rank t1 < rank t2 then t1::merge(ts1',ts2)
        else if rank t2 < rank t2 then t2::merge(ts1,ts2')
             else insTree(link(t1,t2),merge(ts1',ts2'))

  fun removeMinTree [] = raise EMPTY
    | removeMinTree [t] = (t,[])
    | removeMinTree (t::ts) =
        let val (t',ts') = removeMinTree ts
        in if Elem.leq(root t, root t') then (t,ts) else (t',t::ts') end

  fun findMin ts = let val (t,_) = removeMinTree ts in root t end

  fun deleteMin ts =
    let val (Node(_,x,ts1),ts2) = removeMinTree ts
    in merge(rev ts1,ts2) end
end;
