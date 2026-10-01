open Constants

type t = { squares : Color.t array array; us_size : int; opp_size : int }
(** A game board grid and the cached region sizes for each player *)

type player = Us | Opp

let player_to_corner = function
  | Us -> (0, 0)
  | Opp -> Constants.(height - 1, width - 1)

let other_player = function Us -> Opp | Opp -> Us

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

let get_corner board p = get board (player_to_corner p)

let set_corner_arr squares p c =
  let y, x = player_to_corner p in
  squares.(y).(x) <- c

(** [set_corner board player color] sets the color of the corner corresponding
    to [player] to [color] *)
let set_corner board p c = set board (player_to_corner p) c

(** The colors of board corners should never be the same *)
let corner_colors_inv board =
  not (Color.equal (get_corner board Us) (get_corner board Opp))

let height_inv board = Array.length board.squares = height

let width_inv board =
  Array.for_all (fun row -> Array.length row = width) board.squares

let check_inv board =
  assert (corner_colors_inv board);
  assert (height_inv board);
  assert (width_inv board);
  board

(** [neighbors (y, x)] returns all the 4-neighbors
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)] that fit on the board, i.e.
    have first coordinate in [\[0, height)] and second coordinate in
    [\[0, width)] *)
let neighbors (y, x) =
  [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]
  |> List.filter (fun (y', x') ->
      0 <= y' && y' < height && 0 <= x' && x' < width)

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

let of_array squares =
  {
    squares;
    us_size = region_size_naive squares Us;
    opp_size = region_size_naive squares Opp;
  }

(* TODO the game doesn't generate boards with adjacent tiles of the same color. We should do the same *)
let random () =
  let squares = Array.init_matrix height width (fun _ _ -> Color.random ()) in
  let our_color = get_corner_arr squares Us in
  let opp_color = get_corner_arr squares Opp in
  if Color.equal our_color opp_color then
    set_corner_arr squares Us (Color.random_excluding our_color);
  check_inv (of_array squares)

let print board =
  for i = height - 1 downto 0 do
    Array.iter (fun c -> print_string (Color.to_square c)) board.squares.(i);
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
  |> List.rev |> List.map parse_line |> Array.of_list |> of_array |> check_inv

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

let region_size b = function Us -> b.us_size | Opp -> b.opp_size

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
      opp_size = (match p with Opp -> !new_color_count | Us -> old.us_size);
    }

type game_state = Win | Loss | Tie | Not_done

let end_state us opp =
  if us > squares_to_tie then Win
  else if opp > squares_to_tie then Loss
  else if us = opp && us + opp = total_squares then Tie
  else Not_done

let is_done us opp =
  match end_state us opp with Not_done -> false | Win | Loss | Tie -> true
