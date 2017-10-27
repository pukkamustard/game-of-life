open BatPervasives 

(** type for Grid *)
type 'a t =
  'a BatVect.t BatVect.t
  

(** [Grid.make dimx dimy e] returns a grid with first dimension [dimx] and second dimension [dimy]. All the elements in this grid are initialized to [e] *)
let make dimx dimy e =
  BatVect.make dimx (BatVect.make dimy e)

(** [Grid.get x y grid] gets element at position ([x],[y]). *)
let get x y grid =
  BatVect.get (BatVect.get grid x) y


let size grid = 
  (BatVect.length grid, BatVect.length (BatVect.get grid 0))


let safe_get x y grid =
  try
    Some (get x y grid)
  with
    _ -> None

let cyclic_get x y grid =
  let (xdim, ydim) = size grid in
  safe_get (x mod xdim) (y mod ydim) grid

let set x y e grid =
  let col = BatVect.get grid x in
  BatVect.set grid x (BatVect.set col y e)

let map_at x y f grid =
  set x y (f @@ get x y grid) grid

let map f =
  BatVect.map (BatVect.map f)

let mapi f =
  BatVect.mapi (fun x -> BatVect.mapi (fun y e -> f x y e))

let iteri f =
  BatVect.iteri (fun x -> BatVect.iteri (fun y e -> f x y e))

type 'a focus = Focus of int * int * 'a t

let x = function
  | Focus (x,_,_) -> x

let y = function
  | Focus (_,y,_) -> y

let map_focus f = function
  | Focus (x,y,grid) -> Focus (x,y,map f grid)

let map_at_focus f  = function
  | Focus (x,y,grid) -> Focus (x,y,map_at x y f grid)

let focus_on x y grid =
  Focus (x,y,grid)

let extract = function
  | Focus (x,y,grid) -> get x y grid

let duplicate = function
  | Focus (x,y,grid) -> Focus (x,y, mapi (fun x y _ -> focus_on x y grid) grid)

let unfocus = function
  | Focus (_,_,grid) -> grid

let extend f focus =
  map_focus f (duplicate focus)

let shift dx dy = function
  | Focus (x,y,grid) ->
    let max_x = BatVect.length grid in
    let max_y = BatVect.length @@ BatVect.get grid 0 in
      Focus (x + dx |> (min) max_x |> (max) 0, y + dy |> (min) max_y |> (max) 0, grid)

let get_neighbour dx dy = function
  | Focus (x,y,grid) ->
    cyclic_get (x + dx) (y + dy) grid

let set_neighbour dx dy e = function
  | Focus (x,y,grid) -> 
      let (xdim,ydim) = size grid in
      try Focus (x,y, set (x+dx mod xdim) (y+dy mod ydim) e grid)
      with _ -> Focus (x,y,grid)
