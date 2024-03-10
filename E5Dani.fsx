type 'a BinTree =
    Leaf
  | Node of 'a * 'a BinTree * 'a BinTree

// 5.1
let rec inOrder = function
| Leaf -> []
| Node(x, t1,tr) -> (inOrder t1) @ [x] @ (inOrder tr);;

let intBinTree =
    Node(43, Node(25, Node(56,Leaf, Leaf), Leaf),
        Node(562, Leaf, Node(78, Leaf, Leaf)))

inOrder intBinTree

// 5.2
let rec mapInOrder f = function
| Leaf -> Leaf
| Node(x, t1,tr) -> Node(f x, mapInOrder f t1, mapInOrder f tr)

mapInOrder (fun x->x+1) intBinTree


// 5.3
let rec foldInOrder f n a = List.fold f n (inOrder a)

foldInOrder (fun n a -> a + n) 0 intBinTree

// 5.4 & 5.5
type aExp =                     (* Arithmetical expressions *)
    | N of int                  (* numbers *)
    | V of string               (* variables *)
    | Add of aExp * aExp        (* addition *)
    | Mul of aExp * aExp        (* multiplication *)
    | Sub of aExp * aExp        (* subtraction *)

type bExp =                     (* Boolean expressions *)
    | TT                        (* true *)
    | FF                        (* false *)
    | Eq of aExp * aExp         (* equality *)
    | Lt of aExp * aExp         (* less than *)
    | Neg of bExp               (* negation *)
    | Con of bExp * bExp        (* conjunction *)

type stm = (* statements *)
    | Ass of string * aExp (* assignment *)
    | Skip
    | Seq of stm * stm (* sequential composition *)
    | ITE of bExp * stm * stm (* if-then-else *)
    | While of bExp * stm (* while *)
    | IT of bExp * stm (* if-then *)

let rec A a s =
    match a with
    | N n -> n
    | V x -> Map.find x s
    | Add(a1, a2) -> A a1 s + A a2 s
    | Mul(a1, a2) -> A a1 s * A a2 s
    | Sub(a1, a2) -> A a1 s - A a2 s;;

let rec B b s =
    match b with
    | TT -> true
    | FF -> false
    | Eq (b1, b2) -> A b1 s = A b2 s
    | Lt (b1, b2) -> A b1 s < A b2 s
    | Neg (b) -> not (B b s)
    | Con (b1, b2) -> B b1 s && B b2 s

let update x v s = Map.add x v s;;

let rec I stm s =
    match stm with
    | Ass(x,a) -> update x (A a s) s
    | Skip -> s
    | Seq(stm1, stm2) -> I stm2 (I stm1 s)
    | ITE(b,stm1,stm2) -> if B b s then I stm1 s else I stm2 s
    | While(b, stm) ->  if B b s then I stm s else I Skip s;;
    //while B b s do I ITE((b), (stm), (Skip))
    // if B b s then I stm s else I Skip s;;
    // I (ITE((b), while b do I stm s, Skip)) s
    


// Example/evaluations:
let stmt0 = Ass("res",(Add(N 10, N 30)))
let stmt1 = Seq(Ass("res",(Add(N 10, N 30))), Ass("res2",(Add(N 20, N 40))))
let stmt2= Skip
let stmt3 = ITE(TT, Ass("res",(Add(N 10, N 30))), Ass("res2",(Add(N 20, N 40))))
let stmt4 = ITE(FF, Ass("res",(Add(N 10, N 30))), Ass("res2",(Add(N 20, N 40))))


let state0 = Map.empty
let state1 = Map.add"a" 10 state0

I stmt0 state0;;
I stmt1 state0;;
I stmt2 state1;; 
I stmt3 state0;;
I stmt4 state0;;

// 5.6


