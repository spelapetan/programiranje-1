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
