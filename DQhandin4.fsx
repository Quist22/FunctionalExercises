// 4.1
let explode (s:string) = 
    let x = s.ToCharArray()
    let list = List.ofArray(x)
    (list)

explode("Hej")

let rec explode2 (s:string) =  
    match s with
    | "" -> []
    | _ -> s.[0] :: explode2(s.[1..])

explode2("explode")

// 4.2
let implode (s: char list) = List.foldBack(fun c acc -> string c + acc) s ""

implode ['a'; 'b'; 'c']

let implodeRev (s: char list) = List.fold(fun c acc -> string acc + c) "" s 

implodeRev ['a'; 'b'; 'c']

// 4.3
let toUpper s = implode (List.map System.Char.ToUpper (explode(s)))
toUpper "teststring"

let toUpper1 (s:string) = (explode >> List.map System.Char.ToUpper >> implode) s
toUpper1 "itsworking!"

let toUpper2 (s:string) = s |> (implode << List.map System.Char.ToUpper << explode)
toUpper2 "hallo"

// 4.4
let palindrome (s:string) = if s = "" then true else if toUpper(s) = (implodeRev<<explode<<toUpper) s then true else false
palindrome "Anna" 


// 4.5
let rec ack t = 
    match t with
    | (m, n) when m < 0 || n < 0 -> failwith "cannot take negative numbers"
    | (m, n) when m = 0 -> n + 1
    | (m, n) when m > 0 && n = 0 -> ack(m-1, 1)
    | (m, n) when m > 0 && n > 0 -> ack(m-1, ack (m, n-1))
ack(3, 11)
// ack(3, 11) evaluates to 16381

// 4.6
let time f =
    let start = System.DateTime.Now in
    let res = f () in
    let finish = System.DateTime.Now in
    (res, finish - start)

time (fun () -> ack (3,11))

let timeArg1 f a = time (fun _ -> f a)

timeArg1 ack (3,11)

// 4.7
let rec downto1 f (n,e) = if n <= 0 then e else downto1 f (n-1,f(n,e))

let fact n =
    let f (n, acc) = n * acc
    downto1 f (n, 1)

let buildList g n = 
    let g (n, e) = g(n)
    downto1 g (n, 1) :: []
