open Filler
module Board = Board_with_cache
module Checker = Prover.Check (Board)
module Prover = Prover.Minimax (Board)

let string =
  {|🟪🟥🟦⬛🟪🟦🟩🟥                   
    🟦🟨🟩🟨🟥⬛🟦⬛
    🟥🟩🟦🟥⬛🟩⬛🟦
    🟪🟥🟩🟦🟪🟥🟩⬛
    🟩🟦🟪⬛🟨⬛🟨🟪
    🟪⬛🟦🟩🟥🟦🟪🟦
    ⬛🟨🟩🟥🟦🟥🟩🟨 |}

let board = IO.parse_to_squares string |> Board.of_squares
let proof, guarantee = Prover.proof board
let result = Checker.check board proof guarantee
let () = Printf.printf "%b\n" result

(* let board' = move board Color.Red Opp
let () = print board'
let () = print_int (region_size board' Opp)
let () = print_newline () *)
(* let move, (score, done_) = Minimax.minimax board ~player:Opp ~max_depth:3 *)

(* let () =
  Printf.printf "Best move: %s. Score: %f. Done: %b\n" (Color.to_square move)
    score done_ *)
