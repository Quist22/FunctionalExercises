
//exercise 4.1
// The first explode-function uses the built in function for strings to create a char array of characters,
// and then uses the piping operator to pipe these to a List of chars.
let explode (s:string) = 
    s.ToCharArray() |>  List.ofArray;;


//this solution uses the remove property to conduct the solution
let rec explode2 (s:string) = 
    match s with 
    | "" -> []
    | _ -> s.[0] :: explode2 (s.Remove(0, 1))


//exercise 4.2

let (.+) (x1: string) (x2: char) =
    x1 + System.Char.ToString x2

let implode (ls: char list) =
    List.foldBack (fun elem acc -> acc .+ elem) ls ""


