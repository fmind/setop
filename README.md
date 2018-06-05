# Setop: Perform set operations on files

[![hackage](https://img.shields.io/hackage/v/setop.svg)](https://hackage.haskell.org/package/setop)
[![hackage-deps](https://img.shields.io/hackage-deps/v/setop.svg)](https://hackage.haskell.org/package/setop)
 	
## Rationale

[Set operations](https://en.wikipedia.org/wiki/Set_(mathematics)#Basic_operations) are a convenient solution to common problems:

- create a list of tasks without duplicates (set union)
- filter tasks done out of tasks to do (set difference)
- find common elements in a database (set intersection)
- and remove theses elements (set symmetric difference)

Setop helps you run these set operations on your files.

## Usage

Let's introduce two line-separated files: `A.txt` and `B.txt`.

`A.txt` contains all numbers from 0 to 5 included
```text
0
1
2
3
4
5
```

`B.txt` contains all even numbers from 0 to 8 included
```text
0
2
4
6
8
```

### Set Union (U/Union):

```bash
$ setop A.txt U B.txt
0
1
2
3
4
5
6
8
```

### Set Difference (D/Diff):

```bash
$ setop A.txt D B.txt
1
3
5
```

### Set Intersection (I/Inter):

```bash
$ setop A.txt I B.txt
0
2
4
```

### Set Symmetric Difference (J/Disj):

```bash
$ setop A.txt J B.txt
1
3
5
6
8
```

### Reading A.txt from STDIN:

```bash
$ cat A.txt | setop STDIN Diff B.txt
0
2
4
```

### Reading B.txt from STDIN:

```bash
$ cat B.txt | setop A.txt Disj STDIN
1
3
5
6
8
```

## Notes

- the resulting set is __sorted in ascending order__
- some set operations are __not commutative__ (e.g. A Diff B /= B Diff A)
- Setop is based on [optparse-applicative](https://github.com/pcapriotti/optparse-applicative) and supports [bash/fish/zsh completions](https://github.com/pcapriotti/optparse-applicative#bash-zsh-and-fish-completions)
