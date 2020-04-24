Input:
- Num players
- Num marbles

Goal:
- Find the winning score

Insertion rules:
- Numbers inserted in circular list
- Numbers increment by 1 on each turn
- Every number inserted is the _current number_
- Each number is inserted to the right of the current number between distance 1 and 2
- When marble to insert % 23 == 0,
  - current_players's score += number_would_have_inserted + number at list[current - 7]
  - list[current - 7] is removed


**********
SOLUTION A
**********

Input:
- P: Number of players
- N: Number of marbles

State:
- scores[NUM_PLAYERS]
- doubly-linked list with focused item

Initial state:
- All scores 0
- List with 1 item: 0 focused

Algorithm:

for each 1..N as n, loop players circularly as p:
  if n % 23 != 0:
    insert to right after next node from focus
    inserted item becomes new focus
  else:
    scores[p] += n + focus[-7]
    remove focus[-7]

result = max(scores)
