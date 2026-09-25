(** There are two players in the game, and we represent the player that we're
    trying to help win as [Us], and the other player as [Opp] *)
type t =
  | Us  (** The player that we're trying to help win *)
  | Opp  (** The player that we're trying to help lose *)

(** Return [(0, 0)] for [Us], [(height - 1, width - 1)] for [Opp] *)
let to_corner = function
  | Us -> (0, 0)
  | Opp -> Constants.(height - 1, width - 1)

(** The other player, i.e. [Us -> Opp] and [Opp -> Us] *)
let other = function Us -> Opp | Opp -> Us
