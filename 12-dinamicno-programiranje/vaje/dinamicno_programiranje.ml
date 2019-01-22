(* ========== Vaje 6: Dinamično programiranje  ========== *)


(*----------------------------------------------------------------------------*]
 Požrešna miška se nahaja v zgornjem levem kotu šahovnice. Premikati se sme
 samo za eno polje navzdol ali za eno polje na desno in na koncu mora prispeti
 v desni spodnji kot. Na vsakem polju šahovnice je en sirček. Ti sirčki imajo
 različne (ne-negativne) mase. Miška bi se rada kar se da nažrla, zato jo
 zanima, katero pot naj ubere.

 Funkcija [max_cheese cheese_matrix], ki dobi matriko [cheese_matrix] z masami
 sirčkov in vrne največjo skupno maso, ki jo bo miška požrla, če gre po
 optimalni poti.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # max_cheese test_matrix;;
 - : int = 13
[*----------------------------------------------------------------------------*)

let test_matrix = 
  [| [| 1 ; 2 ; 0 |];
     [| 2 ; 4 ; 5 |];
     [| 7 ; 0 ; 1 |] |]


let memoiziraj_rec f =
  let rezultati = Hashtbl.create 512 in
  let rec mem_f x =
    if Hashtbl.mem rezultati x then
      Hashtbl.find rezultati x
    else
      let y = f mem_f x in
      Hashtbl.add rezultati x y;
      y
    
  in
  mem_f



let max_cheese cheese_matrix = 
  let max_row = Array.length cheese_matrix in
  let max_column = Array.length cheese_matrix.(0) in

  let max_cheese' recursive_max_cheese' (r, c) = 
    (* preverimo da sta indeksa na pravih mestih *)
    if r >= max_row || c >= max_column then
      0
    else 
      let right = recursive_max_cheese' (r, (c + 1)) in
      let down = recursive_max_cheese' ((r + 1), c) in
      let our_cheese = cheese_matrix.(r).(c) in
      our_cheese + max right down
  in
  let memoised max_cheese = memoiziraj_rec max_cheese' in
  memoised max_cheese (0, 0)

  
(* brez memoizacije *)
let max_cheese cheese_matrix = 
  let max_row = Array.length cheese_matrix in
  let max_column = Array.length cheese_matrix.(0) in
  
  let max_cheese' r, c = 
    (* preverimo da sta indeksa na pravih mestih *)
    if r >= max_row || c >= max_column then
      0
    else 
      let right = max_cheese' r, (c + 1) in
      let down = max_cheese' (r + 1), c in
      let our_cheese = cheese_matrix.(r).(c) in
      our_cheese + max right down
  in
  max_cheese' 0 0
  
(*----------------------------------------------------------------------------*]
 Rešujemo problem sestavljanja alternirajoče obarvanih stolpov. Imamo štiri
 različne tipe gradnikov, dva modra in dva rdeča. Modri gradniki so višin 2 in
 3, rdeči pa višin 1 in 2.

 Funkcija [alternating_towers] za podano višino vrne število različnih stolpov
 dane višine, ki jih lahko zgradimo z našimi gradniki, kjer se barva gradnikov
 v stolpu izmenjuje (rdeč na modrem, moder na rdečem itd.). Začnemo z gradnikom
 poljubne barve.

 Namig: Uporabi medsebojno rekurzivni pomožni funkciji z ukazom [and].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # alternating_towers 10;;
 - : int = 35
[*----------------------------------------------------------------------------*)


let alternating_towers height =
  let rec redtop height =
    if height <= 0 then 
      0
    else if height <= 2 then 
      1
    else
      bluetop (height-1) + bluetop (height-2)
  and bluetop height =
    if height <= 0 then 
      0
    else if height = 2 then 
      1
    else if height = 3 then 
      2
    else
      redtop (height-2) + redtop (height-3)
  in
redtop height + bluetop height



(*----------------------------------------------------------------------------*)


let articles = [|
	("yoghurt", 0.39, 0.18);
	("milk", 0.89, 1.03);
  ("coffee", 2.19, 0.2);
  ("butter", 1.49, 0.25);
  ("yeast", 0.22, 0.042);
  ("eggs", 2.39, 0.69);
  ("sausage", 3.76, 0.50);
  ("bread", 2.99, 1.0);
  ("Nutella", 4.99, 0.75);
  ("juice", 1.15, 2.0)
|]

let best_value articles max_w =
  (* Choose the item if you can and recursively search further. *)
  let rec get_item acc_w acc_p (_, p, w) =
    if acc_w +. w > max_w then
      (* Item is not suitable, return what we got so far.*)
      acc_p
    else
      (* Find best value after choosing the item. *)
      shopper (acc_w +. w) (acc_p +. p)
  (* Choose every item in the list and return the value of the best choice. *)
  and shopper w p =
    let choices = Array.map (get_item w p) articles in
    Array.fold_left max 0. choices
  in
  shopper 0. 0.

let best_value_unique articles max_w =
  (* Store which items have already been chose in the array [taken]. *)
  (* Choose the item if you can and recursively search further. *)
  let rec get_item taken acc_w acc_p i (_, p, w) =
    if acc_w +. w > max_w || taken.(i) then
      (* Item is not suitable, return what we got so far.*)
      acc_p
    else
      (* Find best value after choosing the item, mark choice in [taken]. *)
      let new_taken = Array.copy taken in
      (new_taken.(i) <- true; shopper new_taken (acc_w +. w) (acc_p +. p))
  (* Choose every item in the list and return the value of the best choice. *)  
  and shopper taken w p =
    let choices = Array.mapi (get_item taken w p) articles in
    Array.fold_left max 0. choices
  in
  let taken = Array.map (fun _ -> false) articles in
shopper taken 0. 0.