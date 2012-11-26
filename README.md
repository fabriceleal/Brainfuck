
# Brainfuck #

A Brainfuck interpreter written in Lisp. Any non-Brainfuck instruction (< > + - [ ] . ,) will be ignored,
which means that you can write human-readable words inside your file (check helloworld.bf).

## Missing ##

* Input (as long as input is missing, rot13.bf wont work!)

## Attention ##

Uses the `quicklisp` system to load the `cl-yacc` package.

## Usage ##

Call it like this:

```shell
./brainfuck.lisp helloworld.bf
```

