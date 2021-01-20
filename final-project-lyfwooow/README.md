specification:

1. The airplane hides under the map in random direction and position. User should only see an
   empty map with 49 white blocks at the beginning.
2. The user clicks a block to uncover it. (the block will change color)
3. Remaining attempts are displayed in the screen.
4. Remaining attempts will start at 10; every time user click a block, remaining attempts will
   decrement.
5. Game mode: There are 3 modes in the game. 
   - "normal mode": no hint will display on the map;
   - "number-mode": the possibility of each block could hide a body of airplane will display in
                   different percentage, will change when block is revealed;
   - "cheat-mode": When a block is revealed, a number of how many blue blocks are around the
                   uncovered block will display.
6. A timer will display to check how long the user spending on the game.
7. When user finds the head of the airplane, the head will explode in animation way and the body
   of airplane will display.

Find the head of airplane:
-You will be given an empty map with 7*7, 49 white blocks. 
-There will be an airplane hiding underneath the map. You need to find the head of the airplane in 5 steps. 
-The airplane might be display in 4 different directions. You could click the block to uncover it. It will show a white bottom layer which means 
the airplane is not here; it could also show a blue bottom layer which means you find the body of the airplane. If it shows a red bottom layer, 
that means you find the head of the airplane and win this game. You need to do some logical inference and imagine the picture to guess where the 
head could be. 
-help mode: give probability of block could be blue. 
-count the blue blocks around the white block. 
-animation: the way of block uncovered, win 
-battle ships 
