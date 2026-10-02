open Constants

(* Supporting types and basic helper functions *)

(** There are two players in the game, and we represent the player that we're
    trying to help win as [Us], and the other player as [Opp] *)
type player =
  | Us  (** The player that we're trying to help win *)
  | Opp  (** The player that we're trying to help lose *)

(** Return [(0, 0)] for [Us], [(height - 1, width - 1)] for [Opp] *)
let player_to_corner = function Us -> (0, 0) | Opp -> (height - 1, width - 1)

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

(** [neighbors (y, x)] returns all the 4-neighbors
    [(y + 1, x), (y - 1, x), (y, x + 1), (y, x - 1)] that fit on the board, i.e.
    have first coordinate in [\[0, height)] and second coordinate in
    [\[0, width)] *)
let neighbors (y, x) =
  [ (y + 1, x); (y - 1, x); (y, x + 1); (y, x - 1) ]
  |> List.filter (fun (y', x') ->
      0 <= y' && y' < height && 0 <= x' && x' < width)

type squares = Color.t array array

module type Get = sig
  type t
  (** The type of 7x8 game boards *)

  val get : t -> int * int -> Color.t
  (** [get board (y, x)] returns the color of the square at index [(y, x)] *)
end

module type S = sig
  include Get

  val of_squares : squares -> t
  val to_squares : t -> squares

  val check_inv : t -> t
  (** Return the input if it satisfies all the invariants, otherwise raise *)

  (* Compute information required for heuristic functions and AI strategies *)

  val region_size : t -> player -> int
  (** Count the size of the colored-in region starting at a corner
      (corresponding to either player) *)

  val move : t -> Color.t -> player -> t
  (** [move board color player] computes the new board if player [player] makes
      move [color] on board [board] *)
end

module Make (M : Get) = struct
  (** [get_corner board player] returns the color of the corner corresponding to
      [player] *)
  let get_corner b p = M.get b (player_to_corner p)

  (** The colors of board corners should never be the same *)
  let corner_colors_inv b =
    not (Color.equal (get_corner b Us) (get_corner b Opp))

  (** Returns [true] for all six colors except those at the two player corners
  *)
  let is_valid_move b c =
    not (Color.equal c (get_corner b Us) || Color.equal c (get_corner b Opp))

  (** Always four valid moves. See [is_valid_move] *)
  let valid_moves b = List.filter (is_valid_move b) Color.all
end
