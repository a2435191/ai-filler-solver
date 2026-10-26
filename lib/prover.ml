type multi =
  | OppMove of (Color.t -> single option)
      (** For a proof that a given strategy is at least so good, we have to show
          a proof for each of the [Opp]'s possible moves. Return None when a
          color is the corner color *)

and single = Done | UsMove of Color.t * multi  (** Make a move *)

module Check (B : Board.S) = struct
  open Board.Make (B)

  let check (board : B.t) (strategy : single) (min_tiles : int) : bool =
    if min_tiles < 0 then
      invalid_arg
        (Printf.sprintf "min_tiles should be >= 0, got %d instead" min_tiles);
    if min_tiles > Constants.total_squares then
      invalid_arg
        (Printf.sprintf "min_tiles should be <= %d, got %d instead"
           Constants.total_squares min_tiles);

    let rec go_single board : single -> bool = function
      | Done -> region_size board Us >= min_tiles
      | UsMove (c, next) ->
          is_valid_move board c && go_multi (move board c Us) next
    and go_multi board : multi -> bool = function
      | OppMove next ->
          List.for_all
            (fun c ->
              match next c with
              | Some single ->
                  is_valid_move board c && go_single (move board c Opp) single
              | None -> not (is_valid_move board c))
            Color.all
    in

    go_single board strategy
end

(** Minimax except we don't reduce the min state *)
module Minimax (B : Board.S) = struct
  open Board.Make (B)

  let proof ?(max_depth = 10) (board : B.t) : single * int =
    if max_depth <= 0 then
      invalid_arg
        (Printf.sprintf "Expected max_depth > 0, got %d instead" max_depth);

    let rec go_us fuel board : single * int =
      assert (fuel >= 1);
      if fuel = 1 then (Done, region_size board Us)
      else
        let moves = valid_moves board in
        let moves_and_scores =
          List.map (fun c -> (c, go_opp (fuel - 1) (move board c Us))) moves
        in
        let c, (next, min_tiles) =
          Minimax.max_of_list
            ~le:(fun (_, (_, t1)) (_, (_, t2)) -> t1 <= t2)
            moves_and_scores
        in
        (UsMove (c, next), min_tiles)
    and go_opp fuel board : multi * int =
      assert (fuel >= 1);
      if fuel = 1 then
        ( OppMove (fun c -> if is_valid_move board c then Some Done else None),
          region_size board Us )
      else
        let multi c : (single * int) option =
          if is_valid_move board c then
            Some (go_us (fuel - 1) (move board c Opp))
          else None
        in
        ( OppMove (fun c -> Option.map fst (multi c)),
          List.fold_left Int.min Constants.total_squares
            (List.map
               (fun c ->
                 Option.fold ~none:Constants.total_squares ~some:snd (multi c))
               Color.all) )
    in

    go_us max_depth board
end
