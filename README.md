# Paraset

Paraset is a parallelized solver for a generalized version of the Game of Set®.

The original Set® is played with a deck of 81 unique cards. Each card has four properties: color, shape, shading, and number of figures. Each property has three values as follows:

- Color: Red, Green, or Purple.
- Shape: Oval, Diamond, or Squiggle.
- Shading: Solid, Shaded, or Open.
- Number of Figures: One, Two, or Three.

A valid set consists of three cards where for each property, the cards must either all have the same value or all different values.
To play the game, deal 12 cards. Any number of players try to find a valid set as fast as they can; when they find it, the three cards are removed and new cards are added.

<img width="368" alt="Screenshot 2023-12-20 at 6 22 01 PM" src="https://github.com/ruan-xian/paraset/assets/72535898/b8bbba80-9557-4538-853e-aee3ac25285f">

We focus on a generalized game of Set^® by creating a game where `C` cards are dealt, with `p` types of properties that each take on `v` possible values. Thus, the classic game of Set uses `C=12`, `p=4`, and `v=3` (and has a deck of `v^p=3^4=81` unique cards). A card `c` has `p` properties, each of which we will denote as `c[i]` for `1 <= i <= p`.
A valid set therefore consists of `v` cards `c_1, c_2, ..., c_v` such that for every property `i`, either `c_1[i] = c_2[i] = ... = c_v[i]` or `c_1[i] != c_2[i] != ... != c_v[i]`.

The problem that we solve is as follows: **Given `C` distinct cards (having different properties), determine all valid sets that can be made from a subset of the `C` cards.**

Paraset was an exploration of parallelization; as such, many versions of our algorithms and parallelizations are available from the executable to run, measure, and compare.

## Example

```
$ stack exec -- paraset-exe -dn -v 5 -f "./test/sampleDeal.txt" 12 3 4 +RTS -N8
Dealt cards:
[[1,1,1,2],[1,2,1,1],[1,2,2,3],[1,2,3,2],[2,1,1,2],[2,1,2,2],[2,1,3,2],[2,2,2,1],[2,2,2,3],[3,1,1,3],[3,3,3,1],[3,3,3,3]]
Solutions:
[[1,2,1,1],[1,2,2,3],[1,2,3,2]]
[[2,1,1,2],[2,1,2,2],[2,1,3,2]]
[[1,2,2,3],[2,1,1,2],[3,3,3,1]]
[[1,1,1,2],[2,2,2,3],[3,3,3,1]]
[[1,2,1,1],[2,1,2,2],[3,3,3,3]]
[[1,1,1,2],[2,2,2,1],[3,3,3,3]]
```

This corresponds to `test/solution.png`.

## Usage

```
  $ stack build
  $ stack exec -- paraset-exe [flags] <cards dealt> <number of values> <number of traits>
```

Flags:

```
  -s     --silent       Silences all output.
  -d     --deck         Prints the deck generated.
  -n     --newline      Prints each solution on a separate line.
  -f     --file=        Optional filename containing dealt cards; otherwise randomly generated
  -r 42  --randseed=42  Sets the random seed used.
  -v 6P  --version=6P   Sets the version used (latest = 6P).
                        Valid options: 6P, 6C, 6, 5, 4, 3, 2, 1
         --help         Print this help message
```

Versions:

1. Sequential Algorithm 1: Brute force combinatorics
2. (Failed) Algo 1 parallelization
3. Algo 1 Parallel preSet evaluation
4. (Failed) card data format experiment
5. Sequential Algorithm 2: Bitstring encoding of Pre-sets
6. Naive parallelization of Algo 2
   6C) Chunked parallelization of Algo 2
   6P) ParBuf parallelization of Algo 2

(V7 is not callable from the executable but for posterity, the module was another failed experiment.)

## Testing

Based on our sample deal, we have test cases checking that each version of our algorithm returns the sets we expect. To run the tests,

```
  $ stack test
```

Note that the test for V4 is commented out since it failed - we didn't end up using this algorithm for our final report/solution since there is something up with the sorting, resulting in extra duplicate sets found.
