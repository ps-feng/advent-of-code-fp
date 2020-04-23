**A** node consists of:

-A header, which is always exactly two numbers:
  - The quantity of child nodes.
  - The quantity of metadata entries.
- Zero or more child nodes (as specified in the header).
- One or more metadata entries (as specified in the header).

Each child node is itself a node that has its own header, child nodes, and metadata. For example:

2 child nodes
| 3 metadata entries          metadata
| |                           | | |
2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2
A----------------------------------
    B----------- C-----------
                     D-----


**********
Solution A
**********

Definitions:
- N: number of child nodes
- M: number of metadata

State:
- Input (we remove items as we consume them)
- Accumulated result

Beginning state:
- Full Input
- Accum = 0

parseMetadata :: Int -> Int -> String -> Int
parseMetadata numMetadata input =
  sum(input[0 .. numMetadata])

parseChildAt :: List Char -> Int -> Int
parseChildAt input accum
  | length input == 0 = accum
  | otherwise =
    let
      (N, input1) = parseNumChildren input
      (M, input2) = parseNumMetadata input1
    in
    case N of
      0 -> sum accum $ parseMetadata M input2
      n -> 
        foldl (\acc input' -> parseChildAt input' acc) accum input2

**********
SOLUTION B
**********

readChild initialInput

readChild : input
  - (rest, N) = readInt input
  - (rest, M) = readInt rest
  - for each child
      fold (\acc _ = (rest, sum) = readChild acc.rest; return (rest, sum + acc.sum) ) }  ) { rest, sumc } 0..N

    - (rest, sumc) = readChild rest
    - sum = sum + sumc
  - for each metadata
    - (rest, value) = readInt rest
    - sum = sum + value
  - return (rest, sum)