type multi =
  | OppMove of single option Iarray.t
      (** For a proof that a given strategy is at least so good, we have to show
          a proof for each of the [Opp]'s possible moves. Return None when a
          color is the corner color. This is effectively a function from [Color]
          to [single option] (the array is indexed by [Color.to_enum]) *)

and single = Done | UsMove of Color.t * multi  (** Make a move *)

let rec proof_size : single -> int = function
  | Done -> 1
  | UsMove (_, OppMove children) ->
      Iarray.fold_left ( + ) 1
        (Iarray.map (Option.fold ~none:1 ~some:proof_size) children)

type certificate = { tree : single; min_tiles : int }
(** A proof tree, starting at a move for [Us], and the claimed lower-bound on
    squares captured for [Us] *)

module Verifier (B : Board.S) = struct
  open Board.Make (B)

  let verify (board : B.t) (claim : certificate) : bool =
    if claim.min_tiles < 0 then
      invalid_arg
        (Printf.sprintf "min_tiles should be >= 0, got %d instead"
           claim.min_tiles);
    if claim.min_tiles > Constants.total_squares then
      invalid_arg
        (Printf.sprintf "min_tiles should be <= %d, got %d instead"
           Constants.total_squares claim.min_tiles);

    let rec go_single board : single -> bool = function
      | Done -> region_size board Us >= claim.min_tiles
      | UsMove (c, next) ->
          is_valid_move board c && go_multi (move board c Us) next
    and go_multi board : multi -> bool = function
      | OppMove next ->
          Iarray.length next = Color.count
          && List.for_all
               (fun c ->
                 match Iarray.get next (Color.to_enum c) with
                 | Some single ->
                     is_valid_move board c
                     && go_single (move board c Opp) single
                 | None -> not (is_valid_move board c))
               Color.all
    in

    go_single board claim.tree
end

(** Minimax except we don't reduce the min state *)
module Minimax (B : Board.S) = struct
  open Board.Make (B)

  let prove ?(max_depth = 10) ?(visited_count = ref 0) (greedy_us_prover : bool)
      (board : B.t) : certificate =
    if max_depth <= 0 then
      invalid_arg
        (Printf.sprintf "Expected max_depth > 0, got %d instead" max_depth);

    let of_fn f = Iarray.of_list (List.map f Color.all) in

    let incr () = incr visited_count in

    let rec go_us fuel board : single * int =
      assert (fuel >= 1);
      incr ();
      if fuel = 1 then (Done, region_size board Us)
      else
        let moves = valid_moves board in
        let moves_and_proofs_and_scores =
          List.map (fun c -> (c, go_opp (fuel - 1) (move board c Us))) moves
        in
        let c, (next, min_tiles) =
          Minimax.max_of_list
            ~le:(fun (_, (_, t1)) (_, (_, t2)) -> t1 <= t2)
            moves_and_proofs_and_scores
        in

        if greedy_us_prover then
          let moves_and_scores =
            List.map (fun c -> (c, region_size (move board c Us) Us)) moves
          in
          (* Pick the greedy move, but use the min_tiles from the proper strategy *)
          let c_greedy, _ =
            Minimax.max_of_list
              ~le:(fun (_, t1) (_, t2) -> t1 <= t2)
              moves_and_scores
          in
          let next_greedy, _ =
            List.assoc c_greedy moves_and_proofs_and_scores
          in
          (UsMove (c_greedy, next_greedy), min_tiles)
        else (UsMove (c, next), min_tiles)
    and go_opp fuel board : multi * int =
      assert (fuel >= 1);
      incr ();
      if fuel = 1 then
        ( OppMove
            (of_fn (fun c -> if is_valid_move board c then Some Done else None)),
          region_size board Us )
      else
        let children : (single * int) option Iarray.t =
          of_fn (fun c ->
              if is_valid_move board c then
                Some (go_us (fuel - 1) (move board c Opp))
              else None)
        in
        ( OppMove (Iarray.map (Option.map fst) children),
          Iarray.fold_left Int.min Constants.total_squares
            (Iarray.map
               (Option.fold ~none:Constants.total_squares ~some:snd)
               children) )
    in

    let tree, min_tiles = go_us max_depth board in
    { tree; min_tiles }
end
