
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

let (+) a b = plusBrit a b;;
let (-) a b = minBrit a b;;
