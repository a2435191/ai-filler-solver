open Constants

type bits = int

type t = {
  colors : bits array;
  us_color : Color.t;
  opp_color : Color.t;
  us_region : bits;
  opp_region : bits;
  us_region_size : int;
  opp_region_size : int;
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

    Lastly, we also precompute the players' regions in [move] and their sizes
    ([Int.popcount]) in [region_size], to avoid duplicating the flood-fill work
    of [move]. [Int.popcount] is not a particularly fast instruction, and it
    makes [region_size] 60% (a few ns) slower, so we compute it once for the
    moved player's region in [move], as opposed to twice in [region_size]. *)

let region_of_player (player : Board.player) board =
  match player with Us -> board.us_region | Opp -> board.opp_region

(** [fill terrain init] flood-fills according to the graph given by the bits of
    [terrain] (as a 2D grid), starting with nodes given by the bit(s) of [init]
*)
let fill (terrain : bits) init =
  (* remove the first bit of each row (byte) *)
  let no_l_col_mask = 0x7F_7F_7F_7F_7F_7F_7F in
  (* remove the last bit of each row (byte) *)
  let no_r_col_mask = 0xFE_FE_FE_FE_FE_FE_FE in

  (* Get all 4-neighbors of the bits in [curr] that are enabled in [terrain] *)
  let step (curr : bits) =
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

let get_aux colors y x =
  let rec get_enum i =
    (* List.find (fun i -> get_bit y x board.(i)) Color.all_enum *)
    assert (i <= Color.max);
    if get_bit y x colors.(i) then i else get_enum (i + 1)
  in
  Color.of_enum_exc (get_enum 0)

(** [get] is quite slow. We only cache the values for the corners *)
let get board (y, x) = get_aux board.colors y x

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

  (* compute everything up-front the first time *)
  let us_color = get_aux out 0 0 in
  let opp_color = get_aux out (height - 1) (width - 1) in

  (* The initial mask for each player's corner. [Us] is the LSB, and [Opp] is the MSB (of the 48 bits). *)
  let us_init = 1 and opp_init = 1 lsl (total_squares - 1) in

  let us_region = fill out.(Color.to_enum us_color) us_init in
  let opp_region = fill out.(Color.to_enum opp_color) opp_init in
  {
    colors = out;
    us_region; opp_region;
    us_color; opp_color;
    us_region_size = Int.popcount us_region;
    opp_region_size = Int.popcount opp_region;
  } [@ocamlformat "disable"]

(* Not to be confused with [1 lsl (total_squares - 1)] above *)
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

let region_size board : Board.player -> int = function
  | Us -> board.us_region_size
  | Opp -> board.opp_region_size

let move board new_color player =
  let old_color = get_corner board player in
  assert (old_color <> new_color);
  let old_color_idx = Color.to_enum old_color in
  let new_color_idx = Color.to_enum new_color in

  let bits = board.colors.(new_color_idx) in
  (* us_region or opp_region *)
  let start = region_of_player player board in

  let reachable = fill bits start in
  let size = Int.popcount reachable in

  let out = Array.copy board.colors in
  out.(old_color_idx) <- out.(old_color_idx) land lnot reachable;
  out.(new_color_idx) <- out.(new_color_idx) lor reachable;

  (* Since a player never captures the other player's tiles, we just update the current player's region *)
  match player with
  | Us ->
      { board with colors = out; us_color = new_color; us_region = reachable; us_region_size = size; } 
      [@ocamlformat "disable"]
  | Opp ->
      { board with colors = out; opp_color = new_color; opp_region = reachable; opp_region_size = size; }
      [@ocamlformat "disable"]
