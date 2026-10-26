#title[AI safety seminar final project proposal]
Will Bradley

= Introduction
This project will create a prover and verifier for the two-player abstract strategy game _Filler_ (by GamePigeon for iOS). In this game, two players compete to capture tiles by flood-filling the board with a color each turn, starting from opposite corners and alternating turns. The first player to capture more than half the squares on the board wins (ties are also possible). Each ply, the player whose turn it is chooses one of four colors: the six total colors in the game minus the two colors chosen the last two plies.

TODO insert image

Valid proofs will show that my player can always capture at least $s$ squares over the course of the game, even if the opponent player makes perfect moves.
My research question is therefore as follows:
#align(
  center,
)[_Can a totally complete, totally sound, deterministic verifier be implemented for the above family of propositions?_]

#let IP = $"IP"$
#let PSPACE = $"PSPACE"$

This is relevant to AI safety because one theorized approach to AI safety is prover-verifier protocols like this. In general, such _proof systems_ may include _interaction_, and it has been shown that the class of problems with _interactive proof systems_ is exactly the class of problems that can be solved by a Turing machine in polynomial space: $IP = PSPACE$. While this protocol differs from a properly interactive proof (it is deterministic, it takes exponential time, and it has no completeness error, meaning there are no valid proofs that the protocol rejects), it is a simple and appropriate first step and building block for further research.

= Related work
I already have a minimax program and support code in OCaml. This was originally for CS 4701 Practicum in Artificial Intelligence, but I dropped the class and am repurposing it. All the code is written by me, with some suggestions from Claude. As mentioned above, it is already a classic result in complexity theory that $IP = PSPACE$. TODO add papers

= Method
Since squares that are captured by a player never become un-captured, the number of such squares never decreases over the course of the game. Therefore, this quantity is naturally compatible with minimax search bounded to a certain depth, which is required because of the branching factor of $4$ in the search tree.

The prover will run a variant of the minimax algorithm returning a proof tree (to a certain depth), and the verifier will check the proof, for a given claim that $\# "captured squares" >= s$. I will also implement two prover functions: a control that always returns a correct proof, and a treatment prover that asserts that the threshold given by the control prover (for the given problem instance) is also correct if my player just looks at the move that _immediately_ captures the most squares (greedy heuristic). I expect the verifier to sometimes reject the treatment prover's "proofs" (exactly when they are incorrect) and always accept the control prover's proofs. If accomplishing both of these is impossible, there must not be any deterministic algorithm for verifying such a proof certificate, and therefore there can't be such an algorithm for _finding_ a certificate either. I would find this extremely surprising.
