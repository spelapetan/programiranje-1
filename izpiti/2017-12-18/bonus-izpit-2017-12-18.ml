(* =================== *)
(* 1. naloga: funkcije *)
(* =================== *)

(* 1.1) Definirajte funkcijo, ki vzame tri cela števila ter vrne njihov produkt.
   Primer: /zmnozi 2 3 4 = 24/ *)
 let zmnozi x y z = x * y * z

(* 1.2) Definirajte funkcijo, ki vzame celo število x in celo število k, ter
   vrne vrednost izraza x^3 + k.
   Primer: /afin_kub 2 1 = 9/ *)
 let afin_kub x k = x * x * x + k

(* 1.3) Definirajte funkcijo, ki vzame seznam in izračuna seznam vrednosti funkcije
   f(x) = x^3 + 2 za elemente vhodnega seznama.
   Primer: /vse_kubiraj_in_pristej_dva [1; 2; 3] = [3; 10; 29]/ *)
 let rec vse_kubiraj_in_pristej_dva f = function
  | [] -> []
  | x :: xs -> f x :: (vse_kubiraj_in_pristej_dva f xs)

(* 1.4) Definirajte funkcijo, ki varno vrne zadnji element seznama v primeru,
   da seznam ni prazen. Uporabite tip option.
   Primer: /zadnji_element [1; 2; 3] = Some 3/ *)
 let rec zadnji_element = function
  | [] -> None
  | x :: [] -> Some(x)
  | x :: xs -> zadnji_element xs

(* 1.5) Definirajte funkcijo, ki izračuna n-to Fibonaccijevo število.
   Pri tem upoštevamo začetna pogoja /fibonacci 0 = 1/ in /fibonacci 1 = 1/.
   Primer: /fibonacci 20 = 10946/ *)
 let rec fibonacci n = 
  match n with
  | 0 -> 1
  | 1 -> 1
  | _ -> fibonacci (n - 1) + fibonacci (n - 2)

(* ======================================= *)
(* 2. naloga: podatkovni tipi in rekurzija *)
(* ======================================= *)

(* 2.1) Rožno drevo je drevo, v katerem ima vsak koren poljubno mnogo otrok,
   ki so zopet rožna drevesa. Rožna drevesa predstavimo s parametričnim
   tipom /'a drevo/ z enim konstruktorjem, ki sprejme:
   - vrednost (koren) tipa /'a/ in
   - seznam (gozd) dreves tipa /'a drevo/. *)

type 'a drevo = Node of 'a * 'a drevo list

(* 2.2) Definirajte naslednja rožna drevesa:

   t = 1,  t' =  2   ,      t'' =  3
                / \               /| \
               t  t              / |  \
                               -1  t'  0

 *)

let t = Node(1, [])
let t' = Node(2, [t; t])
let t'' = Node(3, [Node(-1, []); t'; Node(0, [])])

(* 2.3) Definirajte funkcijo, ki preveri ali je dano rožno drevo list drevesa,
   torej ima prazen gozd poddreves. *)
let je_list list drevo =
  match drevo with
  | (_, []) -> false
  | (_, xs) ->
      List.mem list xs
  

(* 2.4) Definirajte funkcijo, ki preveri, ali drevo celih števil vsebuje zgolj pozitivna števila. *)
let vsa_pozitivna tree =
  match tree with
  | (x, []) -> x > 0
  | (x, y :: ys) ->
    

(* 2.5) Definirajte funkcijo, ki izračuna največjo širino rožnega drevesa, torej največjo dolžino
   gozda, ki se pojavi v kateremkoli vozlišču rožnega drevesa. *)
let sirina_drevesa = failwith "dopolni me"

(* 2.6) Definirajte funkcijo, ki sestavi (poljubno) rožno drevo globine n.
   Vrednosti v korenih so poljubne. *)
let globoko_drevo = failwith "dopolni me"

(* 2.7) Definirajte funkcijo, ki pretvori rožno drevo v seznam. Vrstni red vrednosti v seznamu
   pri tem ni pomemben.
   Primer: /drevo_v_seznam t'' = [3; -1; 2; 1; 1; 0]/ (ali katerakoli permutacija [3; -1; 2; 1; 1; 0])

   Če želite vse točke, mora biti funkcija repno rekurzivna.

   Opomba: kot ste videli na vajah, nekatere funkcije iz modula List,
   na primer List.map, niso repno rekurzivne, zato se jim raje
   izognite. *)
let drevo_v_seznam = failwith "dopolni me"
