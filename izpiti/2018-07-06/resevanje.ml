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

type 'a neprazen_sez = 
  | Konec of 'a
  | Sestavljen of 'a * 'a neprazen_sez

let test = Sestavljen(3, Sestavljen(6, Sestavljen(12, Sestavljen(8, Konec 2))))

(* a *)

let prvi = function
  | Konec a -> a
  | Sestavljen(a, xs) -> a

let rec zadnji = function
  | Konec a -> a 
  | Sestavljen (a, xs) -> zadnji xs

(* b *)

let rec dolzina = function
  | Konec a -> 1
  | Sestavljen (a, xs) -> 1 + dolzina xs

(* c *)

let rec pretvori_v_seznam l =
  let rec aux acc = function
    | Konec a -> List.rev(a :: acc)
    | Sestavljen (a, xs) -> aux (a :: acc) xs
  in
  aux [] l

let rec zlozi f s = function
  | Konec a -> f a
  | Sestavljen (a, xs) -> zlozi f (f s a) xs