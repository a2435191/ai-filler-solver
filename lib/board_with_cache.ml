open Constants
open Board

type t = { squares : squares; us_size : int; opp_size : int }
(** A game board grid and the cached region sizes for each player *)

(* Internal info: a board is represented so that index [(y, x)] corresponds to 
  [y] squares above the bottom row and [x] to the right of the left column.
  The player's position (bottom left in-game) is coordinate [(0, 0)],
  and the opponent's position (upper right in-game) is coordinate [(6, 7)]
  Compared to how this is often done in games/general 2D arrays, this
  is flipped around the y-axis. *)

let get board (y, x) = board.squares.(y).(x)

(** [set board (y, x) color] sets (in-place) the color of the square at index
    [(y, x)] to [color] *)
let set board (y, x) c = board.squares.(y).(x) <- c

let get_corner_arr squares p =
  let y, x = player_to_corner p in
  squares.(y).(x)

let set_corner_arr squares p c =
  let y, x = player_to_corner p in
  squares.(y).(x) <- c

(** [set_corner board player color] sets the color of the corner corresponding
    to [player] to [color] *)
let set_corner board p c = set board (player_to_corner p) c

open Board.Make (struct
  type nonrec t = t

  let get = get
end)

let height_inv board = Array.length board.squares = height

let width_inv board =
  Array.for_all (fun row -> Array.length row = width) board.squares

(** Actually compute the region size when we don't already know it *)
let region_size_naive squares p =
  let visited = Array.make_matrix height width false in
  let c = get_corner_arr squares p in
  let rec count (y, x) =
    visited.(y).(x) <- true;
    List.fold_right
      (fun (y', x') acc ->
        if (not visited.(y').(x')) && Color.equal squares.(y').(x') c then
          acc + count (y', x')
        else acc)
      (neighbors (y, x))
      1
  in
  count (player_to_corner p)

(* Return the precomputed values *)
let region_size b = function Us -> b.us_size | Opp -> b.opp_size
let region_size_inv p b = region_size b p = region_size_naive b.squares p

let check_inv board =
  assert (corner_colors_inv board);
  assert (height_inv board);
  assert (width_inv board);
  assert (region_size_inv Us board);
  assert (region_size_inv Opp board);
  board

let of_squares squares =
  {
    squares;
    us_size = region_size_naive squares Us;
    opp_size = region_size_naive squares Opp;
  }

let to_squares { squares } = squares

(** Deep copy *)
let copy squares = Array.(map copy) squares

let move old new_color p =
  let new_squares = copy old.squares in
  let visited = Array.make_matrix height width false in
  (* corner color *)
  let old_color = get_corner old p in
  let new_color_count = ref (region_size old p) in

  let rec count (y, x) =
    incr new_color_count;
    visited.(y).(x) <- true;
    neighbors (y, x)
    |> List.iter (fun (y', x') ->
        if (not visited.(y').(x')) && Color.equal (get old (y', x')) new_color
        then count (y', x'))
  in

  (* flood fill *)
  let rec fill (y, x) =
    new_squares.(y).(x) <- new_color;
    visited.(y).(x) <- true;
    neighbors (y, x)
    |> List.iter (fun (y', x') ->
        if not visited.(y').(x') then
          let c = get old (y', x') in
          if Color.equal c old_color then fill (y', x')
          else if Color.equal c new_color then count (y', x'))
  in

  fill (player_to_corner p);
  check_inv
    {
      squares = new_squares;
      us_size = (match p with Us -> !new_color_count | Opp -> old.us_size);
      opp_size = (match p with Opp -> !new_color_count | Us -> old.opp_size);
    }
