type t = Red | Green | Yellow | Blue | Purple | Black
[@@deriving show, eq, enum]

let all = [ Red; Green; Yellow; Blue; Purple; Black ]

let to_square = function
  | Red -> "🟥"
  | Green -> "🟩"
  | Yellow -> "🟨"
  | Blue -> "🟦"
  | Purple -> "🟪"
  | Black -> "⬛"

let from_string = function
  | "r" | "R" | "🟥" -> Red
  | "g" | "G" | "🟩" -> Green
  | "y" | "Y" | "🟨" -> Yellow
  | "b" | "B" | "🟦" -> Blue
  | "p" | "P" | "🟪" -> Purple
  | "k" | "K" | "⬛" -> Black
  | other -> raise (Invalid_argument other)

(* Get a uniformly random color *)
let random () = of_enum (Random.int_in_range ~min ~max) |> Option.get

(* [random_excluding c] returns a uniformly random color that is not [c]  *)
let random_excluding c =
  let i = Random.int_in_range ~min ~max:(max - 1) in
  (* Remap everything any index at or above c by adding one *)
  let ret = of_enum (if i >= to_enum c then i + 1 else i) |> Option.get in
  assert (not (equal c ret));
  ret
