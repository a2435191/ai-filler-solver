open Constants

type bits = int

type t = {
  colors : bits array;
  us_color : Color.t;
  opp_color : Color.t;
  us_size : int;
  opp_size : int;
}
(** A bitboard: There are 56 colored squares and 6 possible colors per square.
    Each color needing 3 or 4 bits would take 21 or 28 bytes, but instead I just
    keep 6x 64-bit ints around, one for each color, each bit denoting whether
    its index (when converted to a 2D index) has that color. That's 48 bytes
    total, so a little bigger but it makes [region_size] very fast (see below)

    Since an ocaml [int] has its lowest bit as a tag bit, we only have 63 bits,
    but that's enough. Hence [type bits = int].

    We precompute the players' selected colors so that in [move] we don't have
    to compute them the slow way with [get].

    Lastly, we also precompute the players' region sizes in [move] and just read
    off the values in [region_size], to avoid duplicating the flood-fill work of
    [move]. *)

let corner_bits : Board.player -> bits = function
  | Us -> 1
  | Opp -> 1 lsl (total_squares - 1)

(** Flood-fill according to the graph given by the bits of [terrain] (as a 2D
    grid), starting with nodes given by the bit(s) of [init] *)
let fill terrain init =
  (* remove the first bit of each row (byte) *)
  let no_l_col_mask = 0x7F_7F_7F_7F_7F_7F_7F in
  (* remove the last bit of each row (byte) *)
  let no_r_col_mask = 0xFE_FE_FE_FE_FE_FE_FE in

  (* Get all 4-neighbors of the bits in [curr] that are enabled in [terrain] *)
  let step curr =
    let mask_w = (curr land no_l_col_mask) lsl 1 in
    let mask_e = (curr land no_r_col_mask) lsr 1 in
    let mask_n = curr lsl 8 in
    let mask_s = curr lsr 8 in
    let mask = mask_w lor mask_e lor mask_n lor mask_s in
    curr lor (mask land terrain)
  in

  (* loop until fixed point *)
  let rec go curr =
    let next = step curr in
    if next = curr then curr else go next
  in

  go init

let () = assert (width = 8 && height = 7)
let bit_idx y x = (y * width) + x

let get_bit y x (bits : bits) =
  let i = bit_idx y x in
  bits land (1 lsl i) <> 0

let set_bit y x bit (bits : bits) : bits =
  let i = bit_idx y x in
  if bit then bits lor (1 lsl i) else bits land lnot (1 lsl i)

let get_raw colors y x =
  let rec get_enum i =
    (* List.find (fun i -> get_bit y x board.(i)) Color.all_enum *)
    assert (i <= Color.max);
    if get_bit y x colors.(i) then i else get_enum (i + 1)
  in
  Color.of_enum_exc (get_enum 0)

(** [get] is quite slow. We only cache the values for the corners *)
let get board (y, x) = get_raw board.colors y x

(** fast and cached *)
let get_corner board player =
  match player with Board.Us -> board.us_color | Opp -> board.opp_color

let to_squares board =
  Array.init_matrix height width (fun y x -> get board (y, x))

let of_squares squares =
  let out = Array.make Color.count 0 in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let color = squares.(y).(x) in
      let color_idx = Color.to_enum color in
      out.(color_idx) <- set_bit y x true out.(color_idx)
    done
  done;

  (* compute everything up-front once *)
  let us_color = get_raw out 0 0 in
  let opp_color = get_raw out (height - 1) (width - 1) in
  let us_size =
    Int.popcount (fill out.(Color.to_enum us_color) (corner_bits Us))
  in
  let opp_size =
    Int.popcount (fill out.(Color.to_enum opp_color) (corner_bits Opp))
  in
  { colors = out; us_color; opp_color; us_size; opp_size }

let () = assert ((1 lsl total_squares) - 1 = 0xFF_FF_FF_FF_FF_FF_FF)

let at_least_one_color_inv board =
  let or'ed = Array.fold_left ( lor ) 0 board.colors in
  or'ed = (1 lsl total_squares) - 1

let at_most_one_color_inv board =
  let _, ok =
    Array.fold_left
      (fun (acc, ok) curr -> (acc lor curr, ok && acc land curr = 0))
      (0, true) board.colors
  in
  ok

let no_high_bits_inv board =
  Array.for_all (fun bits -> bits lsr total_squares = 0) board.colors

let check_inv board =
  assert (at_least_one_color_inv board);
  assert (at_most_one_color_inv board);
  assert (no_high_bits_inv board);
  board

let fill_board board player =
  let y, x = Board.player_to_corner player in
  let color = get_corner board player in
  let bits = board.colors.(Color.to_enum color) in
  assert (get_bit y x bits);

  fill bits (corner_bits player)

let region_size board player =
  let reachable = fill_board board player in
  Int.popcount reachable

let move board new_color player =
  let out = Array.copy board.colors in
  let reachable = fill_board board player in

  let old_color_idx = Color.to_enum (get_corner board player) in
  let new_color_idx = Color.to_enum new_color in
  out.(old_color_idx) <- out.(old_color_idx) land lnot reachable;
  out.(new_color_idx) <- out.(new_color_idx) lor reachable;

  match player with
  | Us -> { board with colors = out; us_color = new_color }
  | Opp -> { board with colors = out; opp_color = new_color }
