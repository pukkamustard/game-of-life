open BatPervasives

(** Comonadic Zipper *)

(** The type of the zipper *)
type 'a t =
  | Zipper of  'a list * 'a * 'a list

(** Shift left *)
let shift_left = function
  | Zipper (value :: left, old, right) -> Some (Zipper (left, value, old :: right))
  | Zipper ([],_,_) -> None

(** Shift right *)
let shift_right = function
  | Zipper (left, old, value :: right) -> Some (Zipper (old :: left, value, right))
  | Zipper (_, _, []) -> None


(* Comonad *)

let map f = function
  | Zipper (left, value, right) -> Zipper (List.map f left, f value, List.map f right)

(* aka coreturn *)
let extract = function
  | Zipper (_, value, _) -> value

(* aka cojoin *)
let duplicate =
  let
    duplicate x =
    (x , x)
  in
  function
  | Zipper (_, _, _) as a ->
    Zipper (BatList.unfold a (shift_left %> BatOption.map duplicate)
           , a
           , BatList.unfold a (shift_right %> BatOption.map duplicate)
           )
(* aka cobind *)
let extend f zipper =
  map f (duplicate zipper)

(* Helpers *)

(** Create zipper from List *)
let from_list = function
  | [] -> None
  | hd :: tl -> Some (Zipper ([], hd, tl))

let to_list = function
  | Zipper (left, value, right) -> List.append (List.rev left) (value :: right)

let singleton x =
  Zipper ([], x, [])
