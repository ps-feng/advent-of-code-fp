Input:
- Grid serial number

Goal:
- Calculate cell with maximum total power within a 300x300 grid

Power calculation for (x, y)
- rack_id = x + 10
- power_level = rack_id * 5
- total power = ((power_level + serial_number) * rack_id / 100) % 10 - 5

Algorithm:

Idea: compute summed-area table (https://www.geeksforgeeks.org/summed-area-table-submatrix-summation/)

Any square at in this table has a value that is the sum of all square above and to the left.
       *         *
       *         *
       *         *
****** P ******* Q ********
       *         *
       *         *
       *         *
****** S ******* R ********
       *         *
       *         *
       *         *
AREA SUM = R - Q - S + P

1. Compute summed-area table knowing the power calculation of every square
  - Value at every (x,y) square will be
    SUM(x-1, y) - SUM(x-1, y-1) + SUM(x,y-1)
2. Loop through all squares and keep maximum power