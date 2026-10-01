open Filler
module Board = Board_basic
module Head2head = Head2head.Make (Board)
open Board
open Head2head

let string =
  {|🟪🟥🟦⬛🟪🟦🟩🟥                   
    🟦🟨🟩🟨🟥⬛🟦⬛
    🟥🟩🟦🟥⬛🟩⬛🟦
    🟪🟥🟩🟦🟪🟥🟩⬛
    🟩🟦🟪⬛🟨⬛🟨🟪
    🟪⬛🟦🟩🟥🟦🟪🟦
    ⬛🟨🟩🟥🟦🟥🟩🟨 |}

let board = parse string

(* let () = print board
let () = print_newline () *)
let strategies = [ minimax 1; minimax 5; minimax 10; random; greedy ]

(* TODO: we could consider caching moves here *)
let results = head_to_head ~trials:1 ~boards:(fun _ -> board) strategies
let () = print_results results

(* let board' = move board Color.Red Opp
let () = print board'
let () = print_int (region_size board' Opp)
let () = print_newline () *)
(* let move, (score, done_) = Minimax.minimax board ~player:Opp ~max_depth:3 *)

(* let () =
  Printf.printf "Best move: %s. Score: %f. Done: %b\n" (Color.to_square move)
    score done_ *)
