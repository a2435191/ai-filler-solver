open Filler
module Board = Board_with_cache
module V = Prover.Verifier (Board)
module P = Prover.Minimax (Board)

let () = Random.init 5

let max_depth =
  match
    Option.bind
      (if Array.length Sys.argv = 2 then Some Sys.argv.(1) else None)
      int_of_string_opt
  with
  | None ->
      Printf.eprintf "Usage: ./prog <max_depth>\n";
      exit 1
  | Some x -> x

let mk_boards n = List.init n (fun _ -> Board.of_squares (IO.random_squares ()))
let n_boards = 50
let boards = mk_boards n_boards

type result = {
  accepted : bool;
  proof_size : int;
  n_prover_visited_nodes : int;
  prover_time : float;
  verifier_time : float;
}

let time fn =
  let t0 = Sys.time () in
  let ret = fn () in
  let t1 = Sys.time () in
  (ret, t1 -. t0)

let header = "accepted? | V-cnt  | P-cnt   | P(s)| V(s)"

let check is_treatment board =
  let visited_count = ref 0 in
  let claim, prover_time =
    time (fun () -> P.prove ~max_depth ~visited_count is_treatment board)
  in
  let proof_size = Prover.proof_size claim.tree in
  let accepted, verifier_time = time (fun () -> V.verify board claim) in

  Printf.printf "%s         | %6d | %5d | %.3f | %.3f\n"
    (if accepted then "y" else "n")
    !visited_count proof_size prover_time verifier_time;
  flush stderr;
  {
    accepted;
    proof_size;
    n_prover_visited_nodes = !visited_count;
    prover_time;
    verifier_time;
  }

let () =
  Printf.printf "%s\n" header;

  let control_results = List.map (check false) boards in
  Printf.printf "\n\n";
  let treatment_results = List.map (check true) boards in

  let n_accepted results =
    results |> List.filter (fun r -> r.accepted) |> List.length
  in

  Printf.printf "\n\n";
  Printf.printf "Control (no cheating provers): %d/%d accepted\n"
    (n_accepted control_results)
    n_boards;
  Printf.printf "Treatment (cheating provers): %d/%d accepted\n"
    (n_accepted treatment_results)
    n_boards
