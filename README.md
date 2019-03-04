# sudokuSolver
This Haskell program solves Sudoku puzzles using a Most Constrained Member heuristic.

## Operation
During every cycle, after checking that the current board state is valid, all the unassigned cells are found and sorted in order of what possible values they can take. The one with the fewest possible values is then assigned a value, and another cycle begins, branching with every possible value of this cell. By doing this, we have a higher chance of choosing correct values for cells early on, meaning we will not have to jump out of such a large tree and lose many possibly correct assignments when we run into a bad board state.
