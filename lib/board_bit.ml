open Constants

type t = int array
(** A bitboard: There are 56 colored squares and 6 possible colors per square.
    Each color needing 3 or 4 bits would take 21 or 28 bytes, but instead I just
    keep 6x 64-bit ints around, one for each color, each bit denoting whether
    its index (when converted to a 2D index) has that color. That's 48 bytes
    total, so a little bigger but it makes [region_size] very fast (see below)

    Since an ocaml [int] has its lowest bit as a tag bit, we only have 63 bits,
    but that's enough *)

(* TODO could use 7 leftover bits to save the current player colors rather than having to loop to extract them *)

let bit_idx y x = (y * width) + x

let get_bit y x bits =
  let i = bit_idx y x in
  bits land (1 lsl i) <> 0

let set_bit y x bit bits =
  let i = bit_idx y x in
  if bit then bits lor (1 lsl i) else bits land lnot (1 lsl i)

let get_enum board y x =
  List.find (fun i -> get_bit y x board.(i)) Color.all_enum

let get board (y, x) = Color.of_enum_exc (get_enum board y x)

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
  out

let () = assert ((1 lsl total_squares) - 1 = 0xff_ff_ff_ff_ff_ff_ff)

let at_least_one_color_inv board =
  let or'ed = Array.fold_left ( lor ) 0 board in
  or'ed = (1 lsl total_squares) - 1

let at_most_one_color_inv board =
  let _, ok =
    Array.fold_left
      (fun (acc, ok) curr -> (acc lor curr, ok && acc land curr = 0))
      (0, true) board
  in
  ok

let no_high_bits_inv board =
  Array.for_all (fun bits -> bits lsr total_squares = 0) board

let check_inv board =
  assert (at_least_one_color_inv board);
  assert (at_most_one_color_inv board);
  assert (no_high_bits_inv board);
  board

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

(** Return [(player_color, fill_region)] *)
let fill_board board player =
  let y, x = Board.player_to_corner player in
  let color_idx = get_enum board y x in
  let bits = board.(color_idx) in
  assert (get_bit y x bits);

  let fill_init =
    match player with Us -> 1 | Opp -> 1 lsl (total_squares - 1)
  in

  (color_idx, fill bits fill_init)

let region_size board player =
  let _, reachable = fill_board board player in
  Int.popcount reachable

let move board color player =
  let out = Array.copy board in
  let old_color_idx, reachable = fill_board board player in
  let new_color_idx = Color.to_enum color in

  out.(old_color_idx) <- out.(old_color_idx) land lnot reachable;
  out.(new_color_idx) <- out.(new_color_idx) lor reachable;
  out
