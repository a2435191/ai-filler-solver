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
  let i = Random.int_in_range ~min ~max in
  (* Remap everything any index at or above c by adding one *)
  let ret =
    of_enum (if i = max then min else if i >= to_enum c then i + 1 else i)
    |> Option.get
  in
  assert (not (equal c ret));
  ret
