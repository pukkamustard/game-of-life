open BatPervasives

let sum_of_neighbours = function
  | Zipper.Zipper (hdl::_,x,hdr::_) -> hdl + x + hdr
  | Zipper.Zipper ([], x, hdr::_) -> x + hdr
  | Zipper.Zipper (hdl::_, x, []) -> hdl + x
  | Zipper.Zipper ([], x, []) -> x


let () =
  let zipper =
    [ 0; 0; 0; 0; 1; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 0; 1; 0; 0]
    |> Zipper.from_list
    |> BatOption.default (Zipper.singleton 0)
    |> ref
  in
  while true do
    !zipper
    |> Zipper.to_list
    |> List.map string_of_int
    |> String.concat " "
    |> print_endline;
    zipper := !zipper
              |> Zipper.extend sum_of_neighbours;
    ignore @@ read_line ()
  done

