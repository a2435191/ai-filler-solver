type t = Red | Green | Yellow | Blue | Purple | Black

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
