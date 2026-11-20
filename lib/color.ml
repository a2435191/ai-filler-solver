type t = Red | Green | Yellow | Blue | Purple | Black
[@@deriving show, eq, enum]

(** 6 *)
let count = max + 1

let all = List.init count (fun i -> Option.get (of_enum i))

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
  let avoid = to_enum c in
  (* i has the same range as {0..5} / {avoid}, up to a permutation *)
  (* Remap any index at or above [avoid] by adding one *)
  let ret = Option.get (of_enum (if i < avoid then i else i + 1)) in
  assert (not (equal c ret));
  ret
