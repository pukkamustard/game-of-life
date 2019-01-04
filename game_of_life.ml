open BatPervasives
open Lwt


let living_neighbours grid =
  [ Grid.get_neighbour (-1) (-1) grid
  ; Grid.get_neighbour (-1) (0) grid
  ; Grid.get_neighbour (-1) (1) grid

  ; Grid.get_neighbour (0) (-1) grid
  ; Grid.get_neighbour (0) (1) grid

  ; Grid.get_neighbour (1) (-1) grid
  ; Grid.get_neighbour (1) (0) grid
  ; Grid.get_neighbour (1) (1) grid
  ]
  |> List.filter (BatOption.filter identity %> BatOption.is_some)
  |> List.length

let life_step =
  Grid.extend (fun grid ->
      let alive = Grid.get_neighbour 0 0 grid |> BatOption.default false in
      let n = living_neighbours grid in
      (* underpopulation *)
      if alive && n < 2 then false else
        (* overpopulation *)
      if alive && n > 3 then false else
        (* alive stays alive *)
      if alive then true else
        (* reproduction *)
      if not alive && n = 3 then true
      else false
    )

let draw_glider grid =
  grid
  |> Grid.set_neighbour (1) (-1) true
  |> Grid.set_neighbour (1) (0) true
  |> Grid.set_neighbour (1) (1) true
  |> Grid.set_neighbour (0) (-1) true
  |> Grid.set_neighbour (-1) (0) true

let draw_toad grid =
  grid
  |> Grid.set_neighbour (1) (-1) true
  |> Grid.set_neighbour (1) (0) true
  |> Grid.set_neighbour (1) (1) true
  |> Grid.set_neighbour (0) (0) true
  |> Grid.set_neighbour (0) (1) true
  |> Grid.set_neighbour (0) (2) true


let draw_beacon grid =
  grid
  |> Grid.set_neighbour (-1) (-1) true
  |> Grid.set_neighbour (-1) (0) true
  |> Grid.set_neighbour (0) (-1) true
  |> Grid.set_neighbour (0) (0) true
  |> Grid.set_neighbour (1) (1) true
  |> Grid.set_neighbour (1) (2) true
  |> Grid.set_neighbour (2) (1) true
  |> Grid.set_neighbour (2) (2) true


let randomize_grid =
  Grid.map_focus
    (fun _ ->
       if Random.float 1.0 < 0.33 then
         true
       else
         false
    )

let resize_grid ui grid =
  let size = LTerm_ui.size ui in
  grid := Grid.make (LTerm_geom.rows size) (LTerm_geom.cols size) false
          |> Grid.focus_on 0 0

let is_char uchar chr =
  CamomileLibrary.UChar.eq uchar (CamomileLibrary.UChar.of_char chr)

let rec loop ui grid =
  LTerm_ui.wait ui
  >>= function
  | LTerm_event.Key { code = LTerm_key.Escape; _ } ->
    return ()

  | LTerm_event.Key { code = LTerm_key.Up; _ } ->
    grid := Grid.shift (-1) 0 !grid;
    LTerm_ui.draw ui;
    loop ui grid
  | LTerm_event.Key { code = LTerm_key.Down; _ } ->
    grid := Grid.shift 1 0 !grid;
    LTerm_ui.draw ui;
    loop ui grid
  | LTerm_event.Key { code = LTerm_key.Right; _ } ->
    grid := Grid.shift 0 1 !grid;
    LTerm_ui.draw ui;
    loop ui grid
  | LTerm_event.Key { code = LTerm_key.Left; _ } ->
    grid := Grid.shift 0 (-1) !grid;
    LTerm_ui.draw ui;
    loop ui grid

  | LTerm_event.Key { code = LTerm_key.Char pressed; _ } ->
    (if is_char pressed ' ' then
       grid := life_step !grid

     else if is_char pressed 'd' then
       grid := Grid.map_at_focus not !grid

     else if is_char pressed 'g' then
       grid := draw_glider !grid

     else if is_char pressed 't' then
       grid := draw_toad !grid

     else if is_char pressed 'b' then
       grid := draw_beacon !grid

     else if is_char pressed 'r' then
       grid := randomize_grid !grid

     else if is_char pressed 'c' then
       resize_grid ui grid
    );
    LTerm_ui.draw ui;
    loop ui grid

  | LTerm_event.Key { code = LTerm_key.Enter; _ } ->
    LTerm_ui.draw ui;
    loop ui grid

  | LTerm_event.Resize _ ->
    resize_grid ui grid;
    loop ui grid

  | _ ->
    loop ui grid


let draw grid ui matrix =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  let (cursor_x, cursor_y) = (Grid.x !grid, Grid.y !grid) in
  LTerm_draw.clear ctx;
  Grid.iteri (fun x y active -> if active then
                 LTerm_draw.set_style (LTerm_draw.point ctx x y) { LTerm_style.none with background = Some LTerm_style.white };
             ) (!grid |> Grid.unfocus);
  LTerm_draw.set_style (LTerm_draw.point ctx cursor_x cursor_y) { LTerm_style.none with background = Some LTerm_style.yellow }


let main () =
  let grid = ref @@ Grid.focus_on 0 0 @@ Grid.make 0 0 false in
  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm_ui.create term (draw grid)
  >>= fun ui ->
  resize_grid ui grid;
  Lwt.finalize (fun () -> loop ui grid) (fun () -> LTerm_ui.quit ui)


let () = Lwt_main.run (main ())
