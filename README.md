# wcc

 A wc clone written in Haskell.

## Build

Use [stack](https://docs.haskellstack.org/en/stable/) build tool:

```sh
stack build
```

## Usage

```sh
./wcc-exe -h
wcc - print newline, word, and byte counts for each file

Usage: wcc-exe [-p|--pretty] [FILES...]

  Print newline, word, and byte counts for each FILE, and a total line if more
  than one FILE is specified.

Available options:
  -p,--pretty              Pretty print JSON
  -h,--help                Show this help text
```
