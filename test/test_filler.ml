(* Interface tests for [Board.S].

   Every implementation listed in [Impls.list] is run through the
   same [Suite] functor, and each one becomes its own Alcotest group. Tests
   compare the implementation against [Ref], a deliberately naive reference
   model that works directly on [squares], so they don't trust any of the
   implementations under test.

   - Hand-written edge-case boards are plain Alcotest cases.
   - Everything else is a QCheck2 property over random boards; on failure,
     QCheck shrinks the board (fewer colors, smaller color indices) before
     reporting it.

   Run with [dune test]. Useful knobs:
   - [dune exec test/test_filler.exe -- test Board_bit] runs one implementation
   - QCHECK_SEED=<int> reproduces a failing run (the seed is printed)
   - QCHECK_LONG=1 runs the properties with many more cases *)

open Filler
open Board
open Constants

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

(** Row [height - 1] on top, matching [IO.print_squares]. Tolerates malformed
    (wrong-sized) input so it can be used in failure messages. *)
let show_squares (sq : squares) =
  let buf = Buffer.create 512 in
  Buffer.add_char buf '\n';
  for y = Array.length sq - 1 downto 0 do
    Buffer.add_string buf "  ";
    Array.iter (fun c -> Buffer.add_string buf (Color.to_square c)) sq.(y);
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

let show_player = function Us -> "Us" | Opp -> "Opp"

exception Mismatch of string

let () =
  Printexc.register_printer (function Mismatch msg -> Some msg | _ -> None)

(** Like [Alcotest.check], but logs nothing unless the final result fails.
    [Alcotest.check] and [Alcotest.fail] log every assertion, including every
    failing candidate tried while QCheck shrinks, which buries the minimal
    counterexample under hundreds of lines. *)
let expect ~what ~eq ~show expected actual =
  if not (eq expected actual) then
    raise
      (Mismatch
         (Printf.sprintf "%s\n  expected: %s\n  got:      %s" what
            (show expected) (show actual)))

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
      if in_bounds (y, x) && (not seen.(y).(x)) && Color.equal sq.(y).(x) c then begin
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
    Array.init_matrix height width (fun y x ->
        if r.(y).(x) then c else sq.(y).(x))

  let valid_moves sq =
    List.filter
      (fun c ->
        not
          (Color.equal c (corner_color sq Us)
          || Color.equal c (corner_color sq Opp)))
      Color.all
end

(* ------------------------------------------------------------------------ *)
(* Generators (QCheck2: shrinking is derived from the generator)            *)
(* ------------------------------------------------------------------------ *)

module Gen = struct
  open QCheck2.Gen

  let player = map (fun b -> if b then Us else Opp) bool

  (** A board using only the first [k] colors, [k] in [2, 6]. Fewer colors =>
      bigger regions, which exercises the flood fill much harder than uniform
      6-color boards. Shrinks toward 2 colors and toward [Red]. *)
  let squares : squares t =
    let* k = int_range 2 Color.count in
    let cell = map Color.of_enum_exc (int_bound (k - 1)) in
    let+ sq = array_size (return height) (array_size (return width) cell) in
    (* Corners must differ: deterministically repair rather than filter, so
       shrinking never gets stuck on rejected candidates. *)
    let uy, ux = player_to_corner Us in
    let opp = Ref.corner_color sq Opp in
    if Color.equal sq.(uy).(ux) opp then
      sq.(uy).(ux) <- Color.of_enum_exc ((Color.to_enum opp + 1) mod Color.count);
    sq

  (** A board, a player, and an index into the (always 4) valid moves. Using an
      index rather than a color keeps the move valid while the board shrinks. *)
  let move = triple squares player (int_bound 3)

  (** A board and a sequence of move indices, played alternately from [Us] *)
  let playout = pair squares (list_size (int_range 0 60) (int_bound 3))
end

module Print = struct
  let squares = show_squares

  let move (sq, p, i) =
    Printf.sprintf "%s to move, valid-move index %d, on%s" (show_player p) i
      (show_squares sq)

  let playout (sq, is) =
    Printf.sprintf "moves [%s] on%s"
      (String.concat "; " (List.map string_of_int is))
      (show_squares sq)
end

(* ------------------------------------------------------------------------ *)
(* Hand-written edge cases                                                  *)
(* ------------------------------------------------------------------------ *)

(** [(name, board, (expected Us region, expected Opp region))]. Top line is row
    [height - 1] (the opponent's row); bottom line is row 0 (our row). Letters
    as in [Color.from_string]. *)
let fixed_boards : (string * squares * (int * int)) list =
  let b name sizes s = (name, IO.parse_to_squares s, sizes) in
  [
    (* Us region = all of row 0 plus (1,7). (2,0) is red but isolated: a
       bitboard that lets (1,7) shift east into the next row would reach it. *)
    b "row wrap, east edge (Us)" (9, 1)
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
    b "row wrap, west edge (Us)" (3, 1)
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
    b "row wrap, west edge (Opp)" (1, 9)
      {|
pppppppp
pgbgbgbg
bgbgbgbp
gbgbgbgb
bgbgbgbg
gbgbgbgb
rgbgbgbg
|};
    b "checkerboard" (1, 1)
      {|
grgrgrgr
rgrgrgrg
grgrgrgr
rgrgrgrg
grgrgrgr
rgrgrgrg
grgrgrgr
|};
    b "almost uniform" (55, 1)
      {|
rrrrrrrg
rrrrrrrr
rrrrrrrr
rrrrrrrr
rrrrrrrr
rrrrrrrr
rrrrrrrr
|};
    b "halves" (24, 32)
      {|
gggggggg
gggggggg
gggggggg
gggggggg
rrrrrrrr
rrrrrrrr
rrrrrrrr
|};
    (* One winding region: deep recursion for array impls, many fixed-point
       iterations for the bitboard fill. *)
    b "serpentine" (34, 1)
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

(** The reference model is itself tested against the hand-computed sizes *)
let ref_tests =
  List.map
    (fun (name, sq, (us, opp)) ->
      Alcotest.test_case name `Quick (fun () ->
          Alcotest.(check (pair int int))
            "(Us, Opp) region sizes" (us, opp)
            (Ref.region_size sq Us, Ref.region_size sq Opp)))
    fixed_boards

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

  (* --- Checks shared by unit tests and properties. Each raises with a
     descriptive message on failure (QCheck reports Alcotest's exception). --- *)

  let check_board ~what expected b =
    expect ~what:(what ^ ": to_squares") ~eq:squares_equal ~show:show_squares
      expected (M.to_squares b);
    List.iter
      (fun (y, x) ->
        expect
          ~what:(Printf.sprintf "%s: get (%d, %d)" what y x)
          ~eq:Color.equal ~show:Color.show
          expected.(y).(x)
          (M.get b (y, x)))
      all_cells;
    (* get_corner may be cached, so it gets its own check: after moves, a stale
       cache would disagree with the squares even if [get] is right *)
    List.iter
      (fun p ->
        expect
          ~what:(Printf.sprintf "%s: get_corner %s" what (show_player p))
          ~eq:Color.equal ~show:Color.show (Ref.corner_color expected p)
          (M.get_corner b p))
      [ Us; Opp ];
    List.iter
      (fun p ->
        expect
          ~what:(Printf.sprintf "%s: region_size %s" what (show_player p))
          ~eq:Int.equal ~show:string_of_int
          (Ref.region_size expected p)
          (M.region_size b p))
      [ Us; Opp ]

  let check_fresh sq =
    let b = build sq in
    check_board ~what:"of_squares" sq b;
    check_board ~what:"check_inv" sq (M.check_inv b);
    expect ~what:"valid_moves (Board.Make)" ~eq:(List.equal Color.equal)
      ~show:(fun cs -> String.concat ", " (List.map Color.show cs))
      (Ref.valid_moves sq) (B.valid_moves b)

  let check_move sq p c =
    let what = Printf.sprintf "move %s %s" (Color.show c) (show_player p) in
    check_board ~what (Ref.move sq c p) (M.check_inv (M.move (build sq) c p))

  (** [move] must not mutate its input: the solver relies on boards being
      persistent values. *)
  let check_persistence sq p c =
    let b = build sq in
    ignore (M.move b c p);
    check_board ~what:"original board after move" sq b

  (* --- Unit tests on the fixed boards --- *)

  let fixed_tests =
    List.map
      (fun (name, sq, _) ->
        Alcotest.test_case ("edge case: " ^ name) `Quick (fun () ->
            check_fresh sq;
            List.iter
              (fun p -> List.iter (check_move sq p) (Ref.valid_moves sq))
              [ Us; Opp ]))
      fixed_boards

  (* --- Properties --- *)

  let prop ?(count = 500) name gen print f =
    QCheck_alcotest.to_alcotest ~speed_level:`Quick
      (QCheck2.Test.make ~count ~name ~print gen (fun x ->
           f x;
           true))

  let nth_move sq i = List.nth (Ref.valid_moves sq) i

  let properties =
    [
      prop "of_squares, get, get_corner, region_size, check_inv, valid_moves"
        Gen.squares
        Print.squares check_fresh;
      prop "move matches reference" Gen.move Print.move (fun (sq, p, i) ->
          check_move sq p (nth_move sq i));
      prop "move does not mutate its input" Gen.move Print.move
        (fun (sq, p, i) -> check_persistence sq p (nth_move sq i));
      (* Errors that only appear after several moves, e.g. stale caches *)
      prop ~count:200 "random playouts" Gen.playout Print.playout
        (fun (sq, is) ->
          let rec go sq b p n = function
            | [] -> ()
            | _ when is_done (Ref.region_size sq Us) (Ref.region_size sq Opp) ->
                ()
            | i :: rest ->
                let c = nth_move sq i in
                let sq' = Ref.move sq c p in
                let b' = M.check_inv (M.move b c p) in
                check_board
                  ~what:
                    (Printf.sprintf "playout move %d (%s %s)" n (Color.show c)
                       (show_player p))
                  sq' b';
                go sq' b' (other_player p) (n + 1) rest
          in
          go sq (build sq) Us 0 is);
    ]

  let tests = fixed_tests @ properties
end

(* ------------------------------------------------------------------------ *)
(* Implementations under test: add new ones here or in [Impls.list]         *)
(* ------------------------------------------------------------------------ *)

let impls = Impls.list

let () =
  Alcotest.run "Board.S"
    (("reference model", ref_tests)
    :: List.map
         (fun (name, (module M : Board.S)) ->
           let module T = Suite (M) in
           (name, T.tests))
         impls)
