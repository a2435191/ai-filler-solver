open Constants
open Board

type t = Color.t array array

(* Internal info: a board is represented so that index [(y, x)] corresponds to 
  [y] squares above the bottom row and [x] to the right of the left column.
  The player's position (bottom left in-game) is coordinate [(0, 0)],
  and the opponent's position (upper right in-game) is coordinate [(6, 7)]
  Compared to how this is often done in games/general 2D arrays, this
  is flipped around the y-axis. *)

let to_squares = Fun.id
let of_squares = Fun.id
let get board (y, x) = board.(y).(x)
let height_inv board = Array.length board = height
let width_inv board = Array.for_all (fun row -> Array.length row = width) board

open Board.Make (struct
  type nonrec t = t

  let get = get
end)

let check_inv board =
  assert (corner_colors_inv board);
  assert (height_inv board);
  assert (width_inv board);
  board

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
  count (player_to_corner p)

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

  fill (player_to_corner p);
  check_inv new_
