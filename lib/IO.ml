open Constants
open Board

(* TODO the game doesn't generate boards with adjacent tiles of the same color. We should do the same *)

let random () =
  let ret = Array.init_matrix height width (fun _ _ -> Color.random ()) in
  let us_y, us_x = player_to_corner Us in
  let us_color = ret.(us_y).(us_x) in
  let opp_color =
    let y, x = player_to_corner Opp in
    ret.(y).(x)
  in
  if Color.equal us_color opp_color then
    ret.(us_y).(us_x) <- Color.random_excluding us_color;
  ret

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
  |> List.rev |> List.map parse_line |> Array.of_list
