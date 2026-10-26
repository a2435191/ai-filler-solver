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

let () = Random.init 5
let mk_boards n = List.init n (fun _ -> Board.of_squares (IO.random_squares ()))
let n_boards = 50
let boards = mk_boards n_boards

let check is_treatment board =
  let proof, guarantee = Prover.proof is_treatment board in
  let out = Checker.check board proof guarantee in

  Printf.eprintf "Checked (%b)\n" out;
  flush stderr;
  out

let count_accepted_proofs is_treatment =
  let results = List.map (check is_treatment) boards in
  List.length (List.filter Fun.id results)

let () =
  Printf.printf "Control (no cheating provers): %d/%d accepted\n"
    (count_accepted_proofs false)
    n_boards;
  Printf.printf "Treatment (cheating provers): %d/%d accepted\n"
    (count_accepted_proofs true)
    n_boards

(* let board' = move board Color.Red Opp
let () = print board'
let () = print_int (region_size board' Opp)
let () = print_newline () *)
(* let move, (score, done_) = Minimax.minimax board ~player:Opp ~max_depth:3 *)

(* let () =
  Printf.printf "Best move: %s. Score: %f. Done: %b\n" (Color.to_square move)
    score done_ *)
