open Constants
open Player

type t = Color.t array array

(* Internal info: a board is represented so that index [(y, x)] corresponds to 
  [y] squares above the bottom row and [x] to the right of the left column.
  The player's position (bottom left in-game) is coordinate [(0, 0)],
  and the opponent's position (upper right in-game) is coordinate [(6, 7)]
  Compared to how this is often done in games/general 2D arrays, this
  is flipped around the y-axis. *)

let get board (y, x) = board.(y).(x)
let set board (y, x) c = board.(y).(x) <- c
let get_corner board p = get board (to_corner p)
let set_corner board p c = set board (to_corner p) c

(** The colors of board corners should never be the same *)
let corner_colors_inv board =
  not (Color.equal (get_corner board Us) (get_corner board Opp))

let height_inv board = Array.length board = height
let width_inv board = Array.for_all (fun row -> Array.length row = width) board

let check_inv board =
  assert (corner_colors_inv board);
  assert (height_inv board);
  assert (width_inv board);
  board

(* TODO the game doesn't generate boards with adjacent tiles of the same color. We should do the same *)
let random () =
  let ret = Array.init_matrix height width (fun _ _ -> Color.random ()) in
  let our_color = get_corner ret Us in
  if Color.equal our_color (get_corner ret Opp) then
    set_corner ret Us (Color.random_excluding our_color);
  check_inv ret

let print board =
  for i = height - 1 downto 0 do
    Array.iter (fun c -> print_string (Color.to_square c)) board.(i);
    print_newline ()
  done

let uchar_to_string u =
  let buf = Buffer.create 4 in
  Buffer.add_utf_8_uchar buf u;
  Buffer.contents buf

let parse_line line : Color.t array =
  let len = String.length line in
  let rec go i acc =
    if i >= len then List.rev acc
    else
      let decoded = String.get_utf_8_uchar line i in
      if Uchar.utf_decode_is_valid decoded then
        let i' = i + Uchar.utf_decode_length decoded in
        let s = decoded |> Uchar.utf_decode_uchar |> uchar_to_string in
        if String.trim s = "" then go i' acc
        else
          let c = Color.from_string s in
          go i' (c :: acc)
      else raise (Invalid_argument ("Failed to parse line: " ^ line))
  in
  Array.of_list (go 0 [])

let parse str =
  String.split_all ~sep:"\n" str
  |> List.filter (fun s -> not (String.trim s = ""))
  |> List.rev |> List.map parse_line |> Array.of_list |> check_inv

let is_valid_move_impl us_c op_c c =
  Color.((not (equal c us_c)) && not (equal c op_c))

let is_valid_move b c =
  let us = get_corner b Us in
  let op = get_corner b Opp in
  is_valid_move_impl us op c

let valid_moves b =
  let us = get_corner b Us in
  let op = get_corner b Opp in
  Color.(List.filter (is_valid_move_impl us op) all)

(** [neighbors (y, x)] returns all the 4-neighbors
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)] that fit on the board, i.e.
    have first coordinate in [\[0, height)] and second coordinate in
    [\[0, width)] *)
let neighbors (y, x) =
  [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]
  |> List.filter (fun (y', x') ->
      0 <= y' && y' < height && 0 <= x' && x' < width)

(* TODO this can be combined with `move` *)
let region_size b p =
  let visited = Array.make_matrix height width false in
  let c = get_corner b p in
  let rec count (y, x) =
    visited.(y).(x) <- true;
    List.fold_right
      (fun (y', x') acc ->
        if (not visited.(y').(x')) && Color.equal b.(y').(x') c then
          acc + count (y', x')
        else acc)
      (neighbors (y, x))
      1
  in
  count (to_corner p)

(** Deep copy *)
let copy b = Array.(map copy) b

let move old new_color p =
  let new_ = copy old in
  let visited = Array.make_matrix height width false in
  (* corner color *)
  let old_color = get_corner old p in

  (* flood fill *)
  let rec fill (y, x) =
    new_.(y).(x) <- new_color;
    visited.(y).(x) <- true;
    neighbors (y, x)
    |> List.iter (fun (y', x') ->
        if (not visited.(y').(x')) && Color.equal old.(y').(x') old_color then
          fill (y', x'))
  in

  fill (to_corner p);
  check_inv new_

let is_done us opp =
  us > squares_to_tie || opp > squares_to_tie
  || (us = opp && us + opp = total_squares)
