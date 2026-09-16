type t = Color.t array array

let height = 7
let width = 8
let total_squares = height * width
let squares_to_tie = total_squares / 2
let get_coord board (y, x) = board.(y).(x)
let set_coord board (y, x) c = board.(y).(x) <- c

type player = Us | Opp

let player_to_corner = function Us -> (0, 0) | Opp -> (height - 1, width - 1)
let other_player = function Us -> Opp | Opp -> Us
let get board p = get_coord board (player_to_corner p)
let set board p c = set_coord board (player_to_corner p) c
let corner_colors_inv board = not (Color.equal (get board Us) (get board Opp))
let height_inv board = Array.length board = height
let width_inv board = Array.for_all (fun row -> Array.length row = width) board

let check_inv board =
  assert (corner_colors_inv board);
  assert (height_inv board);
  assert (width_inv board);
  board

let random () =
  let ret = Array.init_matrix height width (fun _ _ -> Color.random ()) in
  let our_color = get ret Us in
  if Color.equal our_color (get ret Opp) then
    set ret Us (Color.random_excluding our_color);
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

let neighbors (y, x) =
  [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]
  |> List.filter (fun (y', x') ->
      0 <= y' && y' < height && 0 <= x' && x' < width)

(* TODO this can be combined with `move`
 * Count the size of the colored-in region starting at a corner *)
let region_size b p =
  let visited = Array.make_matrix height width false in
  let c = get b p in
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

let copy b = Array.(map copy) b

let move old new_color p =
  let new_ = copy old in
  let visited = Array.make_matrix height width false in
  (* corner color *)
  let old_color = get old p in

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
