open Board

(** Heuristic score. + means good for [Us], - means good for [Opp] *)
let score_position b =
  let us = region_size b Us in
  let opp = region_size b Opp in
  assert (us > 0);
  assert (opp > 0);
  assert (us + opp <= total_squares);

  if us > squares_to_tie then infinity
  else if opp > squares_to_tie then neg_infinity
  else float_of_int (us - opp)

let max_fn l f =
  match l with
  | [] -> raise (Invalid_argument "max_fn got passed an empty list")
  | h :: t ->
      List.fold_left
        (fun (e, score) e' ->
          let score' = f e' in
          if score <= score' then (e', score') else (e, score))
        (h, f h)
        t

let min_fn l f = max_fn l (fun x -> -1.0 *. f x)

(** The core minimax algorithm. Returns [(best_score, best_move)] for [Us] *)
let minimax ?(max_depth = 10) board =
  (* Returns the best [(color, score)] for player [p] to make *)
  let rec go fuel b p : Color.t * float =
    assert (fuel >= 0);
    let us_c = get b Us in
    let op_c = get b Opp in
    (* 4 available moves *)
    let moves =
      Color.(
        List.filter (fun c -> (not (equal c us_c)) && not (equal c op_c)) all)
    in

    let score_fn =
      if fuel == 0 then fun c -> score_position (move b c p)
        (* switch to heuristic *)
      else fun c ->
        let _, us_score = go (fuel - 1) (move b c p) (other_player p) in
        us_score
    in

    (* negate this because a positive score is good for [Us], not [Opp] *)
    match p with
    | Us -> max_fn moves score_fn
    | Opp -> min_fn moves score_fn
  in

  go max_depth board Us
