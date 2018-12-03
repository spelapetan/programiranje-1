(* -------- 1 -------- *)

let vsota list =
  let rec vsota_aux list acc =
    match list with
    | [] -> acc
    | x :: xs -> vsota_aux xs (acc + x)
  in
  vsota_aux list 0

(* vsa naravna števila?? *)

(* -------- 2 -------- *)

let rec narascajoce list =
  match list with
  | [] -> true
  | _ :: [] -> true
  | x :: y :: rep -> if x < y then narascajoce (y :: rep) else false

(* -------- 3 -------- *)

let rec vstavi n list =
  match list with
  | [] -> [n]
  | x :: xs -> if x < n then x :: (vstavi n xs) else n :: x :: xs


let uredi list =
  let rec uredi_aux list acc =
    match list with
    | [] -> acc
    | x :: xs -> uredi_aux xs (vstavi x acc)
  in
  uredi_aux list []

(* -------- 4 -------- *)

let novi_uredi cmp list = 
  let rec n_uredi cmp list acc =
    match list with
    | [] -> acc
    | x :: [] -> x :: acc
    | x :: y :: rep -> (cmp x y) 



(* -------- 5 -------- *)


type flyer = { status : status ; name : string }

(*
let flyers = [ {status = Staff; name = "Quinn"}
             ; {status = Passenger (Group 0); name = "Xiao"}
             ; {status = Passenger Top; name = "Jaina"}
             ; {status = Passenger (Group 1000); name = "Aleks"}
             ; {status = Passenger (Group 1000); name = "Robin"}
             ; {status = Staff; name = "Alan"}
             ]
*)

type priority = Top | 

type status = Staff | Passenger

(* -------- 6 -------- *)

let vkrcavanje list =
  match list with
  | [] -> []
  | 


(* -------- 7 -------- *)
