(* PRVA NALOGA *)

(* a *)
let razlika_kvadratov a b =
  let kvadrat_vsote = (a + b) * (a + b) in
  let vsota_kvadratov = a * a + b * b
  in
  kvadrat_vsote - vsota_kvadratov


(* b *)
let rec uporabi_na_paru f (x, y) = (f x, f y)


(* c *)
let rec ponovi_seznam n sez = 
  if n <= 0 then
    []
  else
    sez @ ponovi_seznam (n - 1) sez


(* d *)
let rec razdeli sez =
  let rec raz n_acc p_acc = function
    | [] -> (List.rev n_acc, List.rev p_acc)
    | x :: xs when x < 0 -> raz (x :: n_acc) p_acc xs
    | x :: xs -> raz n_acc (x :: p_acc) xs
  in
  raz [] [] sez


(* DRUGA NALOGA *)

type 'a tree = Empty | Node of 'a tree * 'a * 'a tree

let leaf x = Node (Empty, x, Empty)

let test1 = 
  Node (
    Node(leaf 3, 10, Node (leaf 14, 13, leaf 6)),
    11,
    Node(leaf 2, 8, leaf 10)
  )

let rec padajoca v = function
  | Empty -> []
  | Node (lt, x, rt) when x > v -> []
  | Node (lt, x, rt) ->
      let left = padajoca x lt in
      let right = padajoca x rt in
      if List.length left > List.length right then
        left @ [x]
      else
        right @ [x]

let rec narascajoca v = function
  | Empty -> []
  | Node (lt, x, rt) when x < v -> []
  | Node (lt, x, rt) ->
      let left = narascajoca x lt in
      let right = narascajoca x rt in
      if List.length left > List.length right then
        x :: left
      else
        x :: right

let rec monotona_pot = function
  | Empty -> []
  | Node (lt, x, rt) ->
    let pure_left = monotona_pot lt in
    let pure_right = monotona_pot rt in
    let left_to_right = (padajoca x lt) @ [x] @ (narascajoca x rt) in
    let right_to_left = (narascajoca x lt) @ [x] @ (padajoca x rt) in
    let option = [pure_left; pure_right; left_to_right; right_to_left] in
    let pick_bigger x y =
      if List.length x > List.length y then x else y) in
    List.fold_left pick_bigger pure_left options


(* TRETJA NALOGA *)

type 'a veriga = 
  | Filter of ('a -> bool) * 'a list * 'a veriga
  | Ostalo of 'a list


(* a *)
let test = 
  Filter ((fun x -> x > 0), [],
  Filter ((fun x -> x < 10), [],
  Ostalo []))


(* b *)

let vstavi x veriga =
  match veriga with
  | Ostalo (elementi) -> Osatlo (x :: elementi)
  | Filter (f, elementi, filtri) ->
      if f x then
        Filtri (f, x :: elementi, filtri)
      else
        Filtri (f, elementi, vstavi x filtri)


(* c *)

let rec poisci x =
  | Ostalo elementi -> List.mem x elementi
  | Filter (f, elementi, filtri) ->
      if f x then List.mem x elementi else poisci x filtri


(* d *)
let rec izprazni = function
  | Ostalo elementi -> (Ostalo [], elementi)
  | Filter (f, elementi, filtri) -> 
      let prazni_filtri, pobrani_elementi = izprazni filtri in
      let vsi_elementi = elementi @ pobrani_elementi in
      (Filter (f, [], prazni_filtri), vsi_elementi)


(* e *)

let rec dodaj f veriga =
  let veriga' = Filter(f, [], veriga) in
  let prazna_veriga, elementi = izprazni veriga' in
  List.fold_left (fun v x -> vsatvi x v) prazna_veriga elementi


