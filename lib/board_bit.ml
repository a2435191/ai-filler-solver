open Constants

type t = int array
(** A bitboard: There are 56 colored squares and 6 possible colors per square.
    Each color needing 3 or 4 bits would take 21 or 28 bytes, but instead I just
    keep 6x 64-bit ints around, one for each color, each bit denoting whether
    its index (when converted to a 2D index) has that color. That's 48 bytes
    total, so a little bigger but it makes [region_size] very fast (see below)

    Since an ocaml [int] has its lowest bit as a tag bit, we only have 63 bits,
    but that's enough *)

(** size in bytes *)

let bit_idx y x = (y * width) + x

let get_bit y x bits =
  let i = bit_idx y x in
  bits land (1 lsl i) <> 0

let set_bit y x bit bits =
  let i = bit_idx y x in
  if bit then bits lor (1 lsl i) else bits land lnot (1 lsl i)

let get board (y, x) =
  List.find (fun i -> get_bit y x board.(i)) Color.all_enum |> Color.of_enum_exc

let sorry () = assert false

let to_squares board =
  Array.init_matrix height width (fun y x -> get board (y, x))

let of_squares squares =
  let out = Array.make Color.count 0 in
  for y = 0 to height do
    for x = 0 to width do
      let color = squares.(y).(x) in
      let color_idx = Color.to_enum color in
      out.(color_idx) <- set_bit y x true out.(color_idx)
    done
  done;
  out

let () = assert ((1 lsl total_squares) - 1 = 0xff_ff_ff_ff_ff_ff_ff)

let at_least_one_color board =
  let or'ed = List.fold_left ( lor ) 0 board in
  or'ed = (1 lsl total_squares) - 1

let at_most_one_color board =
  let _, ok =
    List.fold_left
      (fun (prev, ok) curr -> (curr, ok && prev land curr = 0))
      (0, true) board
  in
  ok

let check_inv board =
  assert (at_least_one_color board);
  assert (at_most_one_color board);
  board

let region_size board player = sorry ()
let move = sorry ()
