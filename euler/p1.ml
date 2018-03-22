
(* problem 1
 If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.
*)

(* not tail recursive 
   generates list from [start,endp)
   
*)
let rec range start endp = 
    if start >= endp then []
    else start :: range (start+1) endp;;

let rec trecur_range start endp =
    let rec helper start endp accu =
        if start >= endp then accu
        else helper (start+1) endp (start :: accu) 
    in
        helper start endp [];;
let rec sum_list ls =
    let rec helper ls acc = 
        match ls with
       | [] -> acc
       | (x::xs) -> helper xs (acc+x)
     in
        helper ls 0;;

let multiples = List.filter (fun x -> x mod 3 == 0 || x mod 5 == 0 ) (range 3 1001);;
let answer = sum_list multiples;;

