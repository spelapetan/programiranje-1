(* 1. naloga *)

(* a *)

let rec izpisi_vsa_stevila = function
  | [] -> ()
  | x :: xs -> print_int x; izpisi_vsa_stevila xs


(* b *)

let rec map2_opt l1 l2 f =
  let rec aux l1 l2 f acc =
    match l1, l2 with
    | [], [] -> Some (List.rev acc)
    | _, [] -> None
    | [], _ -> None
    | x :: xs, y :: ys -> 
        let a = f x y in
        aux xs ys f (a :: acc)
  in
  aux l1 l2 f []


(* 2. naloga *)

(* a *)

type filter_tree =
  | Node of int * filter_tree * filter_tree
  | Box of int list

let test =
  Node(10, 
  Node(5, Box [1], Box []), 
  Node(15, Box [], Box [19; 20])
  )

(* b *)

let rec vstavi z tree =
  match tree with
  | Box xs -> Box(z :: xs)
  | Node (x, lt, rt) when z < x -> Node(x, vstavi z lt, rt)
  | Node (x, lt, rt) -> Node(x, lt, vstavi z rt)

(* c *)

let rec vstavi_seznam l tree =
  match l with
  | [] -> tree
  | x :: xs -> vstavi_seznam xs (vstavi x tree)

  let rec boxed_correctly ftree =
    let checker lower upper x =
      match (lower, upper) with
      | (None, None) -> true
      | (Some l, None) -> l <= x
      | (None, Some u) -> x < u
      | (Some l, Some u) -> l <= x && x < u
    in
    let rec values_between lower upper ftree =
      match ftree with
      | Box(xs) -> List.for_all (checker lower upper) xs
      | Node(f, lt, rt) ->
        (values_between lower (Some f) lt) && (values_between (Some f) upper rt)
    in
    values_between None None ftree


(* 3. naloga *)

module type Linear = sig
  (* Osnovni tip modula. *)
  type t
  (* Identiteta. *)
  val id : t
  (* Izračun funkcije na podatkih. *)
  val apply : int*int -> t -> int*int
  (* Funkcija, ki sprejme matriko in jo pretvori v osnovni tip modula.
     Če je osnovni tip modula matrika, pretvori matriko v matriko. *)
  val of_matrix : int*int*int*int -> t
  (* Funkcija, ki sprejme funkcijo in jo pretvori v osnovni tip modula. 
     Če je osnovni tip modula matrika, pretvori funkcijo v matriko. *)
  val of_function : ((int*int) -> (int*int)) -> t
  (* Vrne kompozitum dveh preslikav. *)
  val compose : t -> t -> t
end
 

(* a *)

module Matrika : Linear = struct
  type t = int * int * int * int

  let id = (1, 0, 0, 1)
  let apply (x, y) (a, b, c, d) = (a * x + b * y, c * x + d * y)
  let of_matrix t = t 
  let of_function f =
    let (a, c) = f (1, 0) in
    let (b, d) = f(0, 1) in
    (a, b, c, d)
  let compose (a, b, c, d) (e, f, g, h) =
    (a*e + b*g, a*f + b*h, c*e + d*g, c*f + d*h)
  
end

(* b *)

module Funkcija : Linear = struct
  type t = int * int -> int * int 

  let id = (fun x -> x)

  let apply v f = f v
  let of_matrix (a, b, c, d) = fun (x, y) -> (a*x + b*y, c*x + d*y)
  let of_function f = f
  let compose f g = fun x -> f (g x)

end


(* 4. naloga *)

