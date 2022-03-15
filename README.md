# SICP

## Description
- Another repo for SICP
- Using Brian Harvey's CS61A from spring 2011:
https://people.eecs.berkeley.edu/~bh/61a-pages/

## Setup
This was the best I could find in terms of having the appropriate scheme setup:
https://github.com/theurere/berkeley_cs61a_spring-2011_archive

Basically:
- Install racket, then:

```bash
raco pkg install --auto berkeley
raco pkg install --auto planet-dyoo-simply-scheme1
```

- then you can use whatever editor and using berkeley's scheme library is as easy as:
```scheme
(require berkeley)
```

## Structure
- CS61A skips some exercises and also switches the orders from the first chapter
- Because of that I've split the course and book materials
- all exercises from the book (asked or not in CS61A), are inside `book`
- exercises from the course (homeworks / projects / midterms) are in CS61A
