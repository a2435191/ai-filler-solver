open Filler
module Board = Board_with_cache
module Verifier = Prover.Verifier (Board)
module Prover = Prover.Minimax (Board)

let () = Random.init 5
let mk_boards n = List.init n (fun _ -> Board.of_squares (IO.random_squares ()))
let n_boards = 50
let boards = mk_boards n_boards

let check is_treatment board =
  let claim = Prover.prove is_treatment board in
  let out = Verifier.verify board claim in

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
