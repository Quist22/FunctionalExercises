
//exercise 4.1
// The first explode-function uses the built in function for strings to create a char array of characters,
// and then uses the piping operator to pipe these to a List of chars.
let explode (s:string) = 
    s.ToCharArray() |>  List.ofArray;;


//this solution uses the remove property to conduct the solution
let rec explode2 (s:string) = 
    match s with 
    | "" -> []
    | _ -> s.[0] :: explode2 (s.Remove(0, 1));;


//exercise 4.2

let (.+) (x1: string) (x2: char) =
    x1 + System.Char.ToString x2

let implode (ls: char list) =
    List.foldBack (fun elem acc -> acc .+ elem) ls "";;


let implodeRev (ls: char list) = 
    List.fold (fun (acc: string) (elem: char) -> acc .+ elem) "" ls;;





//exercise 4.3
//solution in one line
let toUpper (s:string) = (explode s) |> List.map System.Char.ToUpper|> implode;;

//using forward function composition
let toUpperforward (s:string) = (explode >> (List.map System.Char.ToUpper) >> implode) s

//using backward funtion composition
let toUpperbackward (s:string) = (implode << (List.map System.Char.ToUpper) << explode) s

//exercise 4.4

let palindrome (s: string) = 
                    if (explode >> (List.map System.Char.ToLower) >> implode) s = (explode >> (List.map System.Char.ToLower) >> implodeRev) s then true else false;;

palindrome "Anna";;
//exercise 4.5

//implementation of ack-function
let rec ack (m, n) = 
        match (m, n) with
        | (0, _) ->  n+1
        | (m, 0) when m > 0 -> ack (m-1, 1)
        | (m, n) when m > 0 && n > 0 -> ack( m-1, ack(m, n-1));;


//exercise 4.6

let time f =
            let start = System.DateTime.Now in
            let res = f () in
            let finish = System.DateTime.Now in
            (res, finish - start);

let r = time (fun () -> ack (3, 11) );;
let result, elapsedTime = r;;
printfn "Result %A" result;;
printfn "Elapsed Time %A" elapsedTime;;



let timeArg1 f a = time (fun () -> ack a);;

let b = timeArg1 ack (3, 11);;


//exercise 4.7

let downto1 f n e =
    if n <= 0 then e
    else
        let sequence = List.rev [1..n]
        List.fold (fun acc x -> f x acc) e sequence
