open Filler
open Board

let string =
  {|🟪🟥🟦⬛🟪🟦🟥🟥                   
    🟦🟨🟩🟨🟥⬛🟦⬛
    🟥🟩🟦🟥⬛🟩⬛🟦
    🟪🟥🟩🟦🟪🟥🟩⬛
    🟩🟦🟪⬛🟨⬛🟨🟪
    🟪⬛🟦🟩🟥🟦🟪🟦
    ⬛🟨🟩🟥🟦🟥🟩🟨 |}

let board = parse string
let () = print board
let () = print_newline ()

(* let board' = move board Color.Red Opp
let () = print board'
let () = print_int (region_size board' Opp)
let () = print_newline () *)
let move, score = Minimax.minimax board ~player:Us ~max_depth:12
let () = Printf.printf "Best move: %s. Score: %f\n" (Color.to_square move) score
