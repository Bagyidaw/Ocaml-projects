
(* RW ocaml destutter list *)

let rec destutter list = 
    match list with
    | []   -> []
    | [hd] -> [hd]
    | hd1 :: hd2 :: tl ->
        if hd1 = hd2 then destutter (hd2::tl)
        else hd1 :: destutter (hd2 :: tl)
    ;;

