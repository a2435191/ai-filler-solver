type strategy = { name : string; f : Board.t -> Player.t -> Color.t }
(** Type of board AIs that select a move, one at a time *)
(* TODO think about whether f should always take Player.t *)

type result = {
  us : strategy;
  opp : strategy;
  us_score : int;
  opp_score : int;
  start_board : Board.t;
  end_board : Board.t;
  plies : int;
}

let scores_inv { us_score; opp_score } =
  us_score >= 0 && opp_score >= 0
  && us_score + opp_score <= Constants.total_squares

let turns_inv { plies } = plies >= 0

let check_result_inv r =
  assert (scores_inv r);
  assert (turns_inv r);
  r

let winner { us_score; opp_score } =
  if us_score > opp_score then Some Player.Us
  else if us_score < opp_score then Some Player.Opp
  else None

let winner_string r =
  match winner r with
  | Some Player.Us -> r.us.name
  | Some Player.Opp -> r.opp.name
  | None -> "<tie>"

(** [minimax max_depth] *)
let minimax max_depth =
  {
    name = "minimax-" ^ string_of_int max_depth;
    f = (fun b player -> fst (Minimax.minimax ~max_depth ~player b));
  }

(** Pick a random valid move *)
let random =
  {
    name = "random";
    f =
      (fun b _ ->
        let i = Random.int 4 in
        List.nth (Board.valid_moves b) i);
  }

(* [greedy Minimax.heuristic _] Should be the same as [minimax 0] *)
let greedy (eval : Board.t -> float) name =
  {
    name = "greedy-" ^ name;
    f =
      (fun b player ->
        let moves = Board.valid_moves b in
        let moves_and_scores =
          List.map (fun c -> (c, eval (Board.move b c player))) moves
        in
        let le (_, score1) (_, score2) = (score1 : float) <= score2 in
        let best, _ =
          match player with
          | Us -> Minimax.max_of_list ~le moves_and_scores
          | Opp -> Minimax.min_of_list ~le moves_and_scores
        in
        best);
  }

(* let cartesian_product l1 l2 =
  List.concat_map (fun a -> List.map (Pair.make a) l2) l1 *)

(** [run_round us opp board] simulates a game between the two strategies [us]
    and [opp] on initial board [board]. [us] goes first. *)
let run_round us opp start_board =
  let open Board in
  let open Player in
  let rec go b player plies =
    let us_score = region_size b Us in
    let opp_score = region_size b Opp in
    (* Printf.printf "%d-%d (%d, %s turn)\n" us_score opp_score plies
      (match player with Us -> "our" | Opp -> "opponent's");
    Board.print b;
    print_newline ();
    flush stdout; *)
    (* TODO we could have them run full games instead of terminating when there's a winner *)
    if is_done us_score opp_score then
      check_result_inv
        { us; opp; us_score; opp_score; start_board; end_board = b; plies }
    else
      let strat = match player with Us -> us | Opp -> opp in
      let c = strat.f b player in
      assert (is_valid_move b c);
      let board' = move b c player in
      go board' (other player) (plies + 1)
  in

  go start_board Us 0

let head_to_head ?(trials = 5) ?(boards = fun _ -> Board.random ()) strategies =
  let strategies = List.to_seq strategies in
  Seq.concat_map
    (fun (us, opp) ->
      Seq.init trials
        (* TODO: have these happen over multiple threads *) (fun i ->
          run_round us opp (boards i)))
    (Seq.product strategies strategies)

let print_results =
  Seq.iter
    (fun { us; opp; us_score; opp_score; start_board; end_board; plies } ->
      Printf.printf "%s (%d) vs. %s (%d) in %d plies:\n" us.name us_score
        opp.name opp_score plies;
      Board.print start_board;
      Printf.printf "->\n";
      Board.print end_board;
      Printf.printf "\n\n")
