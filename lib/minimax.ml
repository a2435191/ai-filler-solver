open Constants
open Board
open Player

let win_score = 10000.0

(** [heuristic us_score opp_score fuel] evaluates how good/bad a game state with
    [us_score] many tiles controlled by [Us] and [opp_score] many controlled by
    [Opp] is. Additionally, for states where one player has already won, we use
    [fuel] (plies remaining) as a bonus, to incentivize winning early. + means
    good for [Us], - means good for [Opp]. This function is used when we don't
    want to go deeper into the search tree, either because we're out of [fuel]
    or because we're in a terminal state (see [Board.is_done]). *)
let heuristic us opp fuel =
  assert (us > 0);
  assert (opp > 0);
  assert (us + opp <= total_squares);

  match Board.end_state us opp with
  | Win -> win_score +. float_of_int fuel
  | Loss -> -.win_score -. float_of_int fuel
  | Tie -> 0.0
  | Not_done -> float_of_int (us - opp)

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
    let score_fn b =
      let us_size = region_size b Us in
      let opp_size = region_size b Opp in

      if fuel = 0 || Board.is_done us_size opp_size then
        (* we're at a leaf node or the game is done, so use the heuristic *)
        heuristic us_size opp_size fuel
      else
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
