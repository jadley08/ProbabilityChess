# Fairy Implementation of Quantum Chess
Done entirely in Racket.

Version1 is a complete implementation of regular chess (fairy implementation) except for checkmate rules. This is due to not wanting those rules in Quantum Chess, to be implemented in later versions.

## Rules as I have chosen to implement:

### Movement
Pieces have two choices, to move normally as a regular piece would, or to make a quantum move.
Upon a quantum move, the piece is split into equal probability of moving and not moving, and is henceforth superimposed.
Note: you cannot capture a piece by using a quantum move.
Note: pawns cannot make quantum moves.


### Capturing
A piece that is known to exist can capture normally, whether taking a known piece or superimposed piece.
A superimposed piece can attempt to make a capture, the probability of it succeeding is equal to the probability of its existence.


### TODO
to be continued?
