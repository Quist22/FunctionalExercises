
//Exercise 3.1
let rec downTo x = 
//function consists of two cases. The first for appending values recursively using the ::-operator.
// The second for appending all the values to an empty list.
                    if x > 0 then 
                        x :: downTo (x-1)
                    else 
                        []

let downTo2 x = 
// similar function to downTo, which creates the same list using pattern matching
//it is noted that a recursive definition is inherant to pattern matching, 
//making it unnecesary to add a recursive marker to the function declaration.
            match x with 
            | 0 -> []
            | _ -> x::downTo (x-1);;



//Exercise 3.2
let rec removeOddIdx x = 
//This function matches a list with at least two elements
// to a list of the first element and the result of the matching on the list minus the first two elements
// The base case is anything else, which matches to an empty list.
                    match x with
                    | x1::x2::xs -> x1::removeOddIdx xs
                    | _ -> []

//Exercise 3.3
let rec combinePair x = 
//This function matches the first to elements of a list to a tuple, which is appended to the following pairs in the list.
//if anything else, the function returns an empty list.
                    match x with
                    | x1::x2::xs -> (x1, x2)::combinePair xs
                    | _ -> []


//Exercise 3.4


//The former British currency had 12 pence to a shilling and 20 shillings to a pound. Declare
//functions to add and subtract two amounts, represented by triples (pounds, shillings, pence) of
//integers, and declare the functions when a representation by records is used. Declare the func-
//tions in infix notation with proper precedences, and use patterns to obtain readable declarations



type british = {
    Pound : int;
    Shilling : int;
    Pence : int;
}

let minBrit (a: british) (b: british) = 

    let (pence: int, penceCarry: int) =

        let penceDifference: int = a.Pence - b.Pence;
        let eval = penceDifference < 0;
        if eval then (penceDifference + 20, 1) else (penceDifference, 0);

    let (shilling: int, shillingCarry) = 

        let shillingDifference : int = a.Shilling - b.Shilling - penceCarry;
        let eval = shillingDifference < 0;
        if eval then (shillingDifference + 12, 1) else (shillingDifference, 0);

    let (pound: int, carry) = 

        let poundDifference : int =  a.Pound - b.Pound - shillingCarry;
        let eval = poundDifference < 0;
        if eval then (poundDifference, 1) else (poundDifference, 0);

    if pound < 0 then failwith "No more british money"

    {Pound = pound; Shilling =  shilling; Pence = pence;}

let plusBrit (a: british) (b: british) = 
    let (pence, carry) =
        let sum = a.Pence + b.Pence
        (sum % 12, sum/12)
    let (shilling, carry) = 
        let sum = a.Shilling + b.Shilling
        (sum % 20, sum / 20)
    let pound = a.Pound + b.Pound + carry
    {Pound = pound; Shilling = shilling; Pence = pence;}

let ( &+ ) a b = plusBrit a b;;
let ( &- ) a b = minBrit a b;;


//Exercise 3.5

//The set of complex numbers is the set of pairs of real numbers. Complex numbers behave almost
//like real numbers if addition and multiplication are defined by:
//(a, b) + (c, d) = (a + c, b + d)
//(a, b) · (c, d) = (ac - bd, bc - ad)
//1. Declare suitable infix functions for addition and multiplication of complex numbers.

//2. The inverse of (a, b) with regard to addition, that is, −(a, b), is (−a, −b), and the inverse of
//(a, b) with regard to multiplication, that is, 1/(a, b), is (a/(a2 + b2 ), −b/(a2 + b2 )) (provided
//that a and b are not both zero). Declare infix functions for subtraction and division of complex
//numbers.

//3. Use let-expressions in the declaration of the division of complex numbers in order to avoid
//repeated evaluation of identical subexpressions.

//part 1:
type complexNum = {
    first: float;
    second: float;
}

let multiplyComplex num1 num2  = (num1.first * num2.first- num1.second * num2.second, num1.second * num2.first - num1.first - num2.second);;
let addComplex num1 num2 = (num1.first + num2.first, num1.second + num2.second);;
2
let ( /+ ) num1 num2 = addComplex num1 num2;;
let ( /* ) num1 num2 = multiplyComplex num1 num2;;

//part 2:

let pow2 num = num * num;;
let subtractComplex num1 num2 = (num1.first - num2.second, num1.second - num2.second);;
let divideComplex num1 num2 = 
            let numIn = {
                first = num2.first/( pow2 num2.first + pow2 num2.second);
                second = -num2.second/( pow2 num2.first + pow2 num2.second);
            }

            num1 /* numIn;

let ( /- ) num1 num2 = subtractComplex num1 num2;
let( /| ) num1 num2 = divideComplex num1 num2;;

//part 3:

//allready done ??


// Exercise 3.6
//Give a declaration for altsum (see Page 76) containing just two clauses.

let rec altsum = function
                | [] -> 0
                | [x] -> x
                | x0::x1::xs -> x0 - x1 + altsum xs;;


let rec altsum2 input = 
                    match input with 
                        | [_] -> if  not input.IsEmpty then  input.Head  else 0
                        | x1::x2::xs -> x1 - x2 + altsum2 xs;;

altsum [1; -1; 2];;
altsum2  [1; -1; 2];;

