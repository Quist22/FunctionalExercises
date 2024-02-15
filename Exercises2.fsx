

let timediff (h1, m1) (h2, m2) = (h2 - h1)*60 + m2 - m1;;

let minutes (h2, m2) = timediff(0, 0) ;;

let rec pow (st: string, n: int) = 
    match n with
    | 0 -> ""
    | _ when n < 0 -> ""
    | _ -> st + pow(st, n-1);;

let rec (!) (num: int) = 
    let num64 = int64 num
    match num64 with
    | 0L -> 1L
    | 1L -> 1L
    | _ when num64 < 0L -> 0L
    | _ -> num64 * (num64-1L);;

let bin (n, k) = !n / (!k * !(n-k));;

!19


//exercise 2.5
    //(1): the type of the function is int*int->int
    //(2): the function terminates when x=0
    //(3): evaluation steps for f(2,3) are:
        //1: f(2,3)
        //2: f(1,6)
        //3: f(0,6)
        //4: 6
    //(4) the function calculates the product of x!*y


//exercise 2.6
    //(1): (bool, int) -> int
    //(2): it returns runtime error as fact(-1) is evaluated
    //(3): It will go directly to else statement through false, as no tuple is evaluated
    //


let curry (f: 'a * 'b -> 'c) a b= f(a, b)

let uncurry (f: 'a -> 'b -> 'c) (a, b)= f a b;;