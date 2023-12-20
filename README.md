# Paraset

Usage: 
```
  $ stack build
  $ stack exec -- paraset-exe [flags] <cards dealt> <number of values> <number of traits>
```
Flags:
```
  -s     --silent       Silences all output.
  -d     --deck         Prints the deck generated.
  -n     --newline      Prints each solution on a separate line.
  -r 42  --randseed=42  Sets the random seed used.
  -v 6P  --version=6P   Sets the version used (latest = 6P). 
                        Valid options: 6P, 6C, 6, 5, 4, 3, 2, 1
         --help         Print this help message
```

Versions:
1) Sequential Algorithm 1
2) (Failed) Algo 1 parallelization
3) Algo 1 Parallel preSet evaluation
4) (Failed) card data format experiment
5) Sequential Algorithm 2
6) Naive parallelization of Algo 2
6C) Chunked parallelization of Algo 2
6P) ParBuf parallelization of Algo 2
