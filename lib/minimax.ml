open Constants
open Board
open Player

let won_score = 10000.0

(** [heuristic board] evaluates how good/bad the position [board] is. + means
    good for [Us], - means good for [Opp]. [infinity]/[neg_infinity] is used to
    denote a state in which we are/the opponent is guaranteed to win,
    respectively. This function is used when we have gone deep into the search
    tree and don't want to go deeper. *)
let heuristic b =
  let us = region_size b Us in
  let opp = region_size b Opp in
  assert (us > 0);
  assert (opp > 0);
  assert (us + opp <= total_squares);

  if us > squares_to_tie then won_score +. float_of_int (us - opp)
  else if opp > squares_to_tie then -.won_score -. float_of_int (opp - us)
  else if us = squares_to_tie && opp = squares_to_tie then 0.0
  else float_of_int (us - opp)

(** [max_of_list ~le lst] computes the maximum element of [lst], where
    comparison [<=] is done by the [le] function ([le x y] returns [true] iff
    [x <= y]). Tiebreaking order is unspecified. Raises for an empty list. *)
let max_of_list ~le = function
  | [] -> raise (Invalid_argument "max_of_list got passed an empty list")
  | h :: t -> List.fold_left (fun acc x -> if le acc x then x else acc) h t

(** [min_of_list ~le lst] is like [max_of_list] but returns the minimum element
    according to [le]. *)
let min_of_list ~le = max_of_list ~le:(Fun.flip le)

(** The core minimax algorithm. See e.g.
    https://wikipedia.org/wiki/Minimax#Minimax_algorithm_with_alternate_moves.

    Returns [(best_move, best_score)] for [player] (default: [Us]), searching at
    most [max_depth] (default: [10]) layers deep. *)
let minimax ?(max_depth = 10) ?(player = Us) board =
  (* Returns the best [(color, score)] for player [p] to make *)
  let rec go fuel b p : Color.t * float =
    assert (fuel >= 0);
    (* 4 available moves *)
    let moves = valid_moves b in

    (* what we use to evaluate moves *)
    let score_fn =
      if fuel = 0 then (* switch to heuristic *) heuristic
      else (* otherwise recurse *) fun b ->
        let _, us_score = go (fuel - 1) b (Player.other p) in
        us_score
    in

    (* Below, we get the move that scores the highest (helps [Us]) if it's our turn, 
      otherwise the move that scores the lowest (helps [Opp])  *)
    let moves_and_scores =
      List.map (fun c -> (c, score_fn (move b c p))) moves
    in
    let le (_, score1) (_, score2) = (score1 : float) <= score2 in

    match p with
    | Us -> max_of_list ~le moves_and_scores
    | Opp -> min_of_list ~le moves_and_scores
  in

  go max_depth board player
