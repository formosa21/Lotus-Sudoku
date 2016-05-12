# Lotus-Sudoku
Each arc and ring must be filled by numbers from 1~7 without repeats.
![Lotus-sudoku](https://scontent-atl3-1.xx.fbcdn.net/t31.0-8/q87/s2048x2048/13063258_1705183119771332_7544352781941827028_o.jpg)

Mark the index of the lotus grid.
![grid](https://scontent-atl3-1.xx.fbcdn.net/v/t1.0-9/13094154_1705179549771689_6711639578273137346_n.jpg?oh=b14948f2484f6fe62e1987c93c3a9775&oe=5799CF9F)

Patterns. 
![patterns](https://scontent-atl3-1.xx.fbcdn.net/v/t1.0-9/11215830_1705179543105023_630103543844397316_n.jpg?oh=6f9c17aa6d0ac9de75f73fc028de4054&oe=57DC0419)



A variable named "test_arr" has been pre-set as an input grid with zeros, following like the sample provided in the slide.

To test the solver, simply go to GHCi, and type in "lotusSolver test_arr" or whatever input-grid other than the "test_arr".

This algorithm used recursive backtracking. A little bit like depth first search. Test all the "holes" with value range from 1 to 7. If one of the value works in that "hole", then it moves on to the next "hole". If all the values does not work in that "hole", then it moves back to previous "hole" that has not been tried all the values (1~7) and increment that filled number.
