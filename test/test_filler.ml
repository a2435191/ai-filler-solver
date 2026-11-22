(* Interface tests for [Board.S].

   Every implementation listed in [impls] (bottom of file) is run through the
   same [Suite] functor. Each test compares the implementation against [Ref], a
   deliberately naive reference model that works directly on [squares], so the
   tests do not trust any of the implementations under test.

   Run with [dune test]. Set FILLER_TEST_SEED=<int> to change the random seed;
   the seed is printed so a failing run can be reproduced. *)

open Filler
open Board
open Constants

(* ------------------------------------------------------------------------ *)
(* Minimal test harness (no external dependencies)                          *)
(* ------------------------------------------------------------------------ *)

exception Check_failed of string

let failf fmt = Printf.ksprintf (fun s -> raise (Check_failed s)) fmt
let passes = ref 0
let failures = ref 0

let run_test name f =
  match f () with
  | () ->
      incr passes;
      Printf.printf "  ok    %s\n%!" name
  | exception Check_failed msg ->
      incr failures;
      Printf.printf "  FAIL  %s\n%s\n%!" name msg
  | exception e ->
      incr failures;
      Printf.printf "  FAIL  %s\n    raised %s\n%!" name (Printexc.to_string e)

(* ------------------------------------------------------------------------ *)
(* Squares helpers                                                          *)
(* ------------------------------------------------------------------------ *)

let copy_squares (sq : squares) : squares = Array.map Array.copy sq

let squares_equal (a : squares) (b : squares) =
  Array.length a = Array.length b
  && Array.for_all2
       (fun ra rb ->
         Array.length ra = Array.length rb && Array.for_all2 Color.equal ra rb)
       a b

(** Render with row [height - 1] on top, matching [IO.print_squares]. Tolerates
    malformed (wrong-sized) input so it can be used in failure messages. *)
let show_squares (sq : squares) =
  let buf = Buffer.create 512 in
  for y = Array.length sq - 1 downto 0 do
    Buffer.add_string buf "    ";
    Array.iter (fun c -> Buffer.add_string buf (Color.to_square c)) sq.(y);
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

let show_player = function Us -> "Us" | Opp -> "Opp"

(* ------------------------------------------------------------------------ *)
(* Reference model                                                          *)
(* ------------------------------------------------------------------------ *)

module Ref = struct
  let in_bounds (y, x) = 0 <= y && y < height && 0 <= x && x < width
  let corner_color (sq : squares) p =
    let y, x = player_to_corner p in
    sq.(y).(x)

  (** Cells 4-connected to [p]'s corner through cells of the corner's color *)
  let region (sq : squares) p =
    let seen = Array.make_matrix height width false in
    let c = corner_color sq p in
    let rec go (y, x) =
      if in_bounds (y, x) && (not seen.(y).(x)) && Color.equal sq.(y).(x) c
      then begin
        seen.(y).(x) <- true;
        List.iter go [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]
      end
    in
    go (player_to_corner p);
    seen

  let region_size sq p =
    Array.fold_left
      (Array.fold_left (fun n b -> if b then n + 1 else n))
      0 (region sq p)

  let move (sq : squares) c p : squares =
    let r = region sq p in
    Array.init_matrix height width (fun y x -> if r.(y).(x) then c else sq.(y).(x))

  let valid_moves sq =
    List.filter
      (fun c ->
        not
          (Color.equal c (corner_color sq Us)
          || Color.equal c (corner_color sq Opp)))
      Color.all
end

(* ------------------------------------------------------------------------ *)
(* Board generators                                                         *)
(* ------------------------------------------------------------------------ *)

(** Random board using only the first [colors] colors (2 <= colors <= 6), with
    the two corners guaranteed different. Fewer colors => bigger regions, which
    exercises the flood fill much harder than uniform 6-color boards. *)
let random_squares ~colors () : squares =
  let pick () = Color.of_enum_exc (Random.int colors) in
  let sq = Array.init_matrix height width (fun _ _ -> pick ()) in
  let uy, ux = player_to_corner Us in
  while Color.equal sq.(uy).(ux) (Ref.corner_color sq Opp) do
    sq.(uy).(ux) <- pick ()
  done;
  sq

(** Hand-written boards targeting specific edge cases. Top line is row
    [height - 1] (the opponent's row); bottom line is row 0 (our row). Letters
    as in [Color.from_string]. *)
let fixed_boards : (string * squares) list =
  let b name s = (name, IO.parse_to_squares s) in
  [
    (* Us region = all of row 0 plus (1,7). (2,0) is red but isolated: a
       bitboard that lets (1,7) shift east into the next row would reach it. *)
    b "row wrap, east edge (Us)"
      {|
gbgbgbgp
bgbgbgbg
gbgbgbgb
bgbgbgbg
rgbgbgbg
gbgbgbgr
rrrrrrrr
|};
    (* Us region = column 0, rows 0-2. (1,7) is red but isolated: a bitboard
       that lets (2,0) shift west into the previous row would reach it. *)
    b "row wrap, west edge (Us)"
      {|
bgbgbgbp
gbgbgbgb
bgbgbgbg
gbgbgbgb
rgbgbgbg
rbgbgbgr
rgbgbgbg
|};
    (* Opp region = all of row 6 plus (5,0). (4,7) is purple but isolated. *)
    b "row wrap, west edge (Opp)"
      {|
pppppppp
pgbgbgbg
bgbgbgbp
gbgbgbgb
bgbgbgbg
gbgbgbgb
rgbgbgbg
|};
    b "checkerboard (all regions size 1)"
      {|
grgrgrgr
rgrgrgrg
grgrgrgr
rgrgrgrg
grgrgrgr
rgrgrgrg
grgrgrgr
|};
    b "almost uniform (Us owns 55)"
      {|
rrrrrrrg
rrrrrrrr
rrrrrrrr
rrrrrrrr
rrrrrrrr
rrrrrrrr
rrrrrrrr
|};
    b "halves (Us 24, Opp 32)"
      {|
gggggggg
gggggggg
gggggggg
gggggggg
rrrrrrrr
rrrrrrrr
rrrrrrrr
|};
    (* A single winding path from (0,0) to near the far corner. Deep
       recursion and many fixed-point iterations for the bitboard fill. *)
    b "serpentine"
      {|
rrrrrrrb
rggggggg
rrrrrrrr
gggggggr
rrrrrrrr
rggggggg
rrrrrrrr
|};
  ]

let random_boards ~n =
  List.concat_map
    (fun colors -> List.init n (fun _ -> random_squares ~colors ()))
    [ 2; 3; 4; 6 ]

(* ------------------------------------------------------------------------ *)
(* The interface test suite                                                 *)
(* ------------------------------------------------------------------------ *)

module Suite (M : Board.S) = struct
  module B = Board.Make (M)

  (* Always hand implementations a private copy, so an implementation that
     aliases its input can't corrupt our expected values (and vice versa). *)
  let build sq = M.of_squares (copy_squares sq)

  let all_cells =
    List.concat_map
      (fun y -> List.init width (fun x -> (y, x)))
      (List.init height Fun.id)

  (* --- individual properties, each checked on a single board --- *)

  let expect_squares ~what ~expected b =
    let actual = M.to_squares b in
    if not (squares_equal expected actual) then
      failf "    %s\n    expected:\n%s    got:\n%s" what (show_squares expected)
        (show_squares actual)

  let expect_get ~what sq b =
    List.iter
      (fun (y, x) ->
        let got = M.get b (y, x) in
        if not (Color.equal got sq.(y).(x)) then
          failf "    %s: get (%d, %d) = %s, expected %s, on\n%s" what y x
            (Color.show got) (Color.show sq.(y).(x)) (show_squares sq))
      all_cells

  let expect_region_sizes ~what sq b =
    List.iter
      (fun p ->
        let expected = Ref.region_size sq p and got = M.region_size b p in
        if got <> expected then
          failf "    %s: region_size %s = %d, expected %d, on\n%s" what
            (show_player p) got expected (show_squares sq))
      [ Us; Opp ]

  let check_roundtrip sq =
    expect_squares ~what:"to_squares (of_squares sq) <> sq" ~expected:sq
      (build sq)

  let check_get sq = expect_get ~what:"after of_squares" sq (build sq)

  let check_check_inv sq =
    let b = M.check_inv (build sq) in
    expect_squares ~what:"check_inv changed the board" ~expected:sq b

  let check_region_size sq = expect_region_sizes ~what:"fresh board" sq (build sq)

  let check_valid_moves sq =
    let b = build sq in
    let got = B.valid_moves b and expected = Ref.valid_moves sq in
    if not (List.equal Color.equal got expected) then
      failf "    valid_moves = [%s], expected [%s], on\n%s"
        (String.concat "; " (List.map Color.show got))
        (String.concat "; " (List.map Color.show expected))
        (show_squares sq)

  (** Every valid move for both players: resulting squares, [get], region
      sizes, and invariants all agree with the reference. *)
  let check_moves sq =
    let b = build sq in
    List.iter
      (fun p ->
        List.iter
          (fun c ->
            let what =
              Printf.sprintf "move %s %s" (Color.show c) (show_player p)
            in
            let expected = Ref.move sq c p in
            let b' = M.check_inv (M.move b c p) in
            (try expect_squares ~what ~expected b'
             with Check_failed m ->
               failf "%s\n    from:\n%s" m (show_squares sq));
            expect_get ~what expected b';
            expect_region_sizes ~what expected b')
          (Ref.valid_moves sq))
      [ Us; Opp ]

  (** [move] must not mutate its input: the solver relies on boards being
      persistent values. *)
  let check_persistence sq =
    let b = build sq in
    let before_us = M.region_size b Us and before_opp = M.region_size b Opp in
    List.iter
      (fun p -> List.iter (fun c -> ignore (M.move b c p)) (Ref.valid_moves sq))
      [ Us; Opp ];
    expect_squares ~what:"original board changed after calling move"
      ~expected:sq b;
    if M.region_size b Us <> before_us || M.region_size b Opp <> before_opp then
      failf "    region_size of original board changed after calling move"

  (** Play a random game to the end, checking against the reference after
      every move. Catches errors that only show up after several moves (e.g.
      stale cached state). *)
  let check_playout sq =
    let rec go sq b p moves =
      let us = Ref.region_size sq Us and opp = Ref.region_size sq Opp in
      if is_done us opp || moves > 200 then ()
      else
        let options = Ref.valid_moves sq in
        let c = List.nth options (Random.int (List.length options)) in
        let what =
          Printf.sprintf "playout move %d (%s %s)" moves (Color.show c)
            (show_player p)
        in
        let sq' = Ref.move sq c p in
        let b' = M.check_inv (M.move b c p) in
        expect_squares ~what ~expected:sq' b';
        expect_region_sizes ~what sq' b';
        go sq' b' (other_player p) (moves + 1)
    in
    go sq (build sq) Us 0

  (* --- run each property over the fixed and random boards --- *)

  let run () =
    let boards = List.map snd fixed_boards @ random_boards ~n:50 in
    let over_all prop () = List.iter prop boards in
    run_test "of_squares / to_squares round-trip" (over_all check_roundtrip);
    run_test "get matches squares" (over_all check_get);
    run_test "check_inv accepts valid boards" (over_all check_check_inv);
    run_test "region_size matches reference" (over_all check_region_size);
    run_test "valid_moves (via Board.Make)" (over_all check_valid_moves);
    run_test "move matches reference (all moves, both players)"
      (over_all check_moves);
    run_test "move does not mutate its input" (over_all check_persistence);
    run_test "random playouts to game end" (over_all check_playout);
    (* Named edge cases again, separately, so a failure points at the case *)
    List.iter
      (fun (name, sq) ->
        run_test ("edge case: " ^ name) (fun () ->
            check_roundtrip sq;
            check_region_size sq;
            check_moves sq))
      fixed_boards
end

(* ------------------------------------------------------------------------ *)
(* Implementations under test: add new ones here                            *)
(* ------------------------------------------------------------------------ *)

let impls : (string * (module Board.S)) list =
  [
    ("Board_basic", (module Board_basic));
    ("Board_with_cache", (module Board_with_cache));
    ("Board_bit", (module Board_bit));
  ]

let () =
  let seed =
    match Sys.getenv_opt "FILLER_TEST_SEED" with
    | Some s -> int_of_string s
    | None -> 0xF111E
  in
  Printf.printf "seed = %d\n%!" seed;
  List.iter
    (fun (name, (module M : Board.S)) ->
      (* reseed per implementation so every impl sees the same boards *)
      Random.init seed;
      Printf.printf "\n== %s\n%!" name;
      let module T = Suite (M) in
      T.run ())
    impls;
  Printf.printf "\n%d passed, %d failed\n" !passes !failures;
  if !failures > 0 then exit 1
