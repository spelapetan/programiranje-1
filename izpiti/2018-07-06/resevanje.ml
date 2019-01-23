(* 1. naloga *)

(* a *)

let uporabi f x = f x

(* b *)

let ibaropu x f = f x

(* c *) 

let rec zacetnih n xs = 
  let rec aux n xs acc =
    match n, xs with
    | x, ys when x <= 0 -> Some(List.rev acc)
    | x, [] when x > 0 -> None
    | x, y :: ys -> aux (x - 1) ys (y :: acc)
  in
  aux n xs []


(* 2. naloga *)
