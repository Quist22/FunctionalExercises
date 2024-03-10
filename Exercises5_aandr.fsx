

//Exercise 5.1

//Firstly, we define a BinTree datatype for us to start working on:

type 'a BinTree =
    | Leaf
    | Node of 'a * 'a BinTree * 'a BinTree

//we define a function to traverse the tree in order
//the function is based on the preOrder function from slide 32
let rec inOrder tree =
    match tree with
        Leaf -> []
        | Node(n,treeL,treeR) ->
        //as we are need to perform array appending on the elements but cannot use the cons-operator, we put n inside of a list
        inOrder treeL @ [n] @ inOrder treeR;

//to test that the function works, we define a variable of type BinTree and test the function:
let intBinTree =
    Node(43, Node(25, Node(56, Leaf, Leaf), Leaf),
        Node(562, Leaf, Node(78, Leaf, Leaf)))

inOrder intBinTree
//It works !!


//Exercise 5.2

//the mapInOrder function is based on the same recursive logic as inOrder, 
//but maps a Leaf to a Leaf and a Node to a Node instead, where the function is applied to any values stored in the newly constructed nodes.

let rec mapInOrder (f: 'a -> 'b) (tree: 'a BinTree) = 
                match tree with 
                    Leaf -> Leaf
                    | Node(n, treeL, treeR) ->
                        Node(f n, mapInOrder f treeL, mapInOrder f treeR);;

mapInOrder (fun x -> x + 1) intBinTree;;
//and it works :)



//Exercise 5.3


