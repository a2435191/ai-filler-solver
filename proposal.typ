#title[AI safety seminar final project proposal]
Will Bradley

= Introduction
This project will create a prover and verifier for the two-player abstract strategy game _Filler_ (by GamePigeon for iOS). In this game, two players compete to capture tiles by flood-filling the board with a color each turn, starting from opposite corners and alternating turns. The first player to capture more than half the squares on the board wins (ties are also possible). Each ply, the player whose turn it is chooses one of four colors: the six total colors in the game minus the two colors chosen the last two plies.

TODO insert image

Valid proofs will show that our player can always capture at least $s$ squares over the course of the game, even if the opponent player makes perfect moves.
Our research question is therefore as follows:
#align(
  center,
)[_Can a totally complete, totally sound, deterministic verifier be implemented for the above family of propositions?_]

This is relevant to AI safety because one theorized approach to AI safety is prover-verifier protocols like this. In general, such _proof systems_ may include _interaction_, and it has been shown that the class of problems with _interactive proof systems_ is exactly the class of problems that can be solved by a Turing machine in polynomial space: $"IP" = "PSPACE"$. While this protocol differs from a properly interactive proof (it has no completeness error, meaning there are no valid proofs that the protocol rejects, and it is deterministic), it is a simple and appropriate first step and building block for further research.

= Related work
I already have a minimax program and TODO

= Method
Since this quantity never decreases over the course of the game, it is naturally compatible with minimax search bounded to a certain depth, which is required because of the branching factor of $4$ in the search tree.

The prover will run a variant of the minimax algorithm returning a proof tree, and the verifier will check the proof, for a given claim that TODO.
