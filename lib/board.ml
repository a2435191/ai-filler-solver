type t = Color.t array array

let height = 7
let width = 8
let get_ll board = board.(0).(0)
let get_ur board = board.(height - 1).(width - 1)
let set_ll board x = board.(0).(0) <- x
let set_ur board x = board.(height - 1).(width - 1) <- x
let corner_colors_inv board = not (Color.equal (get_ll board) (get_ur board))
let height_inv board = Array.length board == height
let width_inv board = Array.for_all (fun row -> Array.length row == width) board
let inv board = corner_colors_inv board && height_inv board && width_inv board

let check_inv board =
  assert (inv board);
  board

let random () =
  let ret = Array.init_matrix height width (fun _ _ -> Color.random ()) in
  if Color.equal (get_ll ret) (get_ur ret) then
    set_ll ret (Color.random_excluding (get_ll ret));
  check_inv ret

let print board =
  for i = height - 1 downto 0 do
    Array.iter (fun c -> print_string (Color.to_square c)) board.(i);
    print_newline ()
  done
