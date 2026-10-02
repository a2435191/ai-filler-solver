(* Supporting types and basic helper functions *)

(** There are two players in the game, and we represent the player that we're
    trying to help win as [Us], and the other player as [Opp] *)
type player =
  | Us  (** The player that we're trying to help win *)
  | Opp  (** The player that we're trying to help lose *)

(** Return [(0, 0)] for [Us], [(height - 1, width - 1)] for [Opp] *)
let player_to_corner = function
  | Us -> (0, 0)
  | Opp -> Constants.(height - 1, width - 1)

(** The other player, i.e. [Us -> Opp] and [Opp -> Us] *)
let other_player = function Us -> Opp | Opp -> Us

type game_state =
  | Win  (** A win for [Us] *)
  | Loss  (** A win for [Opp], loss for [Us] *)
  | Tie  (** [Us] and [Opp] both have exactly [squares_to_tie] tiles *)
  | Not_done
      (** At this state, there's not a guaranteed win or a guaranteed tie *)

(** [end_state us_size opp_size] *)
let end_state us opp =
  let open Constants in
  if us > squares_to_tie then Win
  else if opp > squares_to_tie then Loss
  else if us = opp && us + opp = total_squares then Tie
  else Not_done

(** [is_done us_size opp_size] returns [true] iff the game has a known winner
    (even if there are squares not yet captured), i.e. [us_size] or [opp_size]
    is [> squares_to_tie], or if both players are tied and all squares are
    captured *)
let is_done us opp =
  match end_state us opp with Not_done -> false | Win | Loss | Tie -> true

type squares = Color.t array array

(* TODO the game doesn't generate boards with adjacent tiles of the same color. We should do the same *)

(** Return a uniform random board *with corners of different colors* *)
let random () : squares =
  let ret =
    Array.init_matrix Constants.height Constants.width (fun _ _ ->
        Color.random ())
  in
  let us_y, us_x = player_to_corner Us in
  let us_color = ret.(us_y).(us_x) in
  let opp_color =
    let y, x = player_to_corner Opp in
    ret.(y).(x)
  in
  if Color.equal us_color opp_color then
    ret.(us_y).(us_x) <- Color.random_excluding us_color;
  ret

(** Pretty-print board to stdout *)
let print (board : squares) =
  for i = Constants.height - 1 downto 0 do
    Array.iter (fun c -> print_string (Color.to_square c)) board.(i);
    print_newline ()
  done

(** Parse a newline-delimited string as a board. Accepts the format output by
    [print], or letters for each of the colors (see [Color.from_string]) *)
let parse str : squares =
  let parse_line line : Color.t array =
    let len = String.length line in

    let uchar_to_string u =
      let buf = Buffer.create 4 in
      Buffer.add_utf_8_uchar buf u;
      Buffer.contents buf
    in

    let rec go i acc =
      if i >= len then List.rev acc
      else
        let decoded = String.get_utf_8_uchar line i in
        if Uchar.utf_decode_is_valid decoded then
          let i' = i + Uchar.utf_decode_length decoded in
          let s = decoded |> Uchar.utf_decode_uchar |> uchar_to_string in
          if String.trim s = "" then go i' acc
          else
            let c = Color.from_string s in
            go i' (c :: acc)
        else raise (Invalid_argument ("Failed to parse line: " ^ line))
    in
    Array.of_list (go 0 [])
  in

  String.split_all ~sep:"\n" str
  |> List.filter (fun s -> not (String.trim s = ""))
  |> List.rev |> List.map parse_line |> Array.of_list

module type Board = sig
  type t
  (** The type of 7x8 game boards *)

  val of_squares : squares -> t
  val to_squares : t -> squares

  val get : t -> int * int -> Color.t
  (** [get board (y, x)] returns the color of the square at index [(y, x)] *)

  val get_corner : t -> player -> Color.t
  (** [get_corner board player] returns the color of the corner corresponding to
      [player] *)

  val check_inv : t -> t
  (** Return the input if it satisfies all the invariants, otherwise raise *)

  (* Compute information required for heuristic functions and AI strategies *)

  val is_valid_move : t -> Color.t -> bool
  (** Returns [true] for all six colors except those at the two player corners
  *)

  val valid_moves : t -> Color.t list
  (** Always four valid moves. See [is_valid_move] *)

  val region_size : t -> player -> int
  (** Count the size of the colored-in region starting at a corner
      (corresponding to either player) *)

  val move : t -> Color.t -> player -> t
  (** [move board color player] computes the new board if player [player] makes
      move [color] on board [board] *)
end
