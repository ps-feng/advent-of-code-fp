 -->A--->B--
 /    \      \
C      -->D----->E
 \           /
  ---->F-----

G ---- H

Sample input:
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.

Result: CABDFE

Final solution chosen: D

----

**********
Solution A
**********

Weight: int(letter) + (if childIsTerminal then 50 else 0)
Terminal is not added

1. Visit C -> [A(1), F(6 + 50)]
2. Visit A -> [B(2 + 50), D(4 + 50), F(6 + 50)]
3. Visit B -> [D(4 + 50), F(6 + 50)] -- E not added because it's termina
4. Visit D -> [F(6 + 50)]
5. Visit F -> []
CABDFE

Complexity: 
- Parsing: ?
- Inserting/Retrieving from priority queue: O(log n)
- Going through every node: O(n)
- Total complexity: O(n log n)

Parsing:
type Node =
  { tag :: Char
  , weight: Int
  , children: List
  }

type NodeMap = Map Char Node

**********
Solution B
**********
  A B C D E F
A 0 1 0 1 0 0
B 0 0 0 0 1 0
C 1 0 0 0 0 1
D 0 0 0 0 1 0
E 0 0 0 0 0 0
F 0 0 0 0 1 0
  A           B
[ 0 0 1 0 0 0 1 0 0 0 0 0 ]

rows 
column 

1. Build the adjacency map (column: 1 if row is parent)
  2. Scan the first column without adjacencies - that's our root (C)
  3. Remove row
  4. Repeat from 2 until no more rows are left

Complexity:
- Parsing:

**********
Solution C
**********

Step 1:
Give nodes a weight composed of:
- Level: every level is +100, starting at 0 for roots
- Order of the letter in alphabet so that A = 0, C = 3 and so on

C - 0 + 3
G - 0 + 7
A - 100 + 1
F - 100 + 6
B - 200 + 2
D - 200 + 4
E - 300 + 5

Step2:
Sort nodes by weight.

Complexity:
- Parsing: ?
- Calculating order: O(n log n)

Parsing:

1. Fill a map of characters to Node that represents the hierarchy.
type Node =
  { name :: Char
  , weight: Int
  , children: List Node
  }

type NodeMap = Map Char Node

2. Find out roots


**********
Solution D
**********

The problem is about doing a topological sort of the given nodes so that:
	- A node is visited when there's no more previous dependency.In other words: it's in-degree is 0
	- In case of multiple choices, get the one with the lowest alphabetical order

Adjacency list:
G -> A
C -> A, F
A -> B, D
B -> E
D -> E
F -> E

Starting state
------------------
Counter of in-degrees for every letter: {  G: 0, C: 0, A: 2, B: 1, D: 1, F: 1, E: 3 }
Priority queue of roots ordered alphabetically (in-degree = 0): [C, G]

Steps
-------

	1. Take C from roots. Roots: [G]
	Adj of C: A, F. 
	Counter(A)--, Counter(F)--. Roots: [A, F, G]
	Result: [C]

	2. Take A from roots. Roots: [F, G]
	Adj of A: B, D
	Counter(B)--, Counter(D)--. Roots: [B, D, F, G]
	Result: [C, A]

	3. Take B from roots. Roots: [D, F, G]
	Adj of B: E
	Counter( E)--. Roots: [D, F, G]
	Result: [C, A, B]
	
	4. Take D from roots. Roots: [F, G]
	Adj of D: E
	Counter( E)--. Roots: [F, G]
	Result: [C, A, B, D]
	
	5. Take F from roots. Roots: [G]
	Adj of F: E
	Counter( E)--. Roots: [E, G]
	Result:  [C, A, B, D, F]
	
	6. Take E from roots. Roots: [G]
	Adj of E: Nothing
	Roots: [G]
	Result: [C, A, B, D, F, E]
	
	7. Take G from roots. Roots: []
	Adj of G: Nothing
	Roots: []
	Result: [C, A, B, D, F, E, G]
