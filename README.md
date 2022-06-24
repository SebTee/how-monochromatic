# how-monochromatic

[![Haskell CI](https://github.com/SebTee/how-monochromatic/actions/workflows/haskell.yml/badge.svg)](https://github.com/SebTee/how-monochromatic/actions/workflows/haskell.yml)

A tool to analyse [bi-coloured graphs](https://mariokrenn.wordpress.com/graph-theory-question/) and determine how close to being monochromatic they are.

[Documentation](https://sebtee.github.io/how-monochromatic/)

## Installation Guide

You need to have the [Haskell Tool Stack](https://haskellstack.org) installed.

Run the following commands:
```bash
git clone https://github.com/SebTee/how-monochromatic.git
cd ./how-monochromatic
stack install
```

## User Guide

The installed executable can be run by running `howmono` on the command line.
It takes no command line arguments.
It only reads a string encoded bi-coloured graph from the stdin.
The string encoding of the bi-coloured graph is defined in the
[ParseBCG documentation](https://sebtee.github.io/how-monochromatic/how-monochromatic-0.1.0.0/ParseBCG.html#v:parse)
(See the example `g.txt` file below).

The program will return a value between 0 and 1.
The closer to 1 the returned value is the closer to being monochromatic the input bi-coloured graph is.

### Example Command

The following command reads the bi-coloured graph from the `g.txt` file and 
pipes it into the stdin of the `howmono` executable.

```bash
cat ./g.txt | howmono
```

Contents of `g.txt`
```
1 green 2 green 1 0
1 blue  3 blue  1 0 
1 red   4 green 0 1 
1 red   6 red   1 0 
2 red   3 red   1 0 
2 blue  5 blue  1 0 
3 green 4 green 1 0 
3 green 6 red   0 1 
4 red   5 red   1 0 
4 red   6 green 0 1 
4 blue  6 blue  1 0 
5 green 6 green 1 0
```

This example will return `0.5`.