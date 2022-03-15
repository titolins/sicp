# [SICP](https://mitpress.mit.edu/sites/default/files/sicp/index.html)

## Description
- Another repo for SICP
- Using Brian Harvey's CS61A from spring 2011:
https://people.eecs.berkeley.edu/~bh/61a-pages/

## Setup
Since the course uses a custom scheme library, I've decided to do a dual setup.

### Book
- For the book, the idea was to have something bare bone, as close to [MIT scheme](https://www.gnu.org/software/mit-scheme/) as possible, but with better editor support / REPL, etc..
- Eventually, I came across [Guile](https://www.gnu.org/software/guile/) and stucked to it
- Minor difference I've noticed so far is in terms of guile not providing a `nil` object
  - instead just use `'()` as recommended by the book later on

### Course
- The course requires the berkeley library
- They do provide an old code to the library in the archive above, but loading that straight away fails in every scheme implementation I've tried
- Eventually, I found [this repo](https://github.com/theurere/berkeley_cs61a_spring-2011_archive) with really awesome instructions
- Basically, they have adapted the code in a racket library
- Install racket, then:

```bash
raco pkg install --auto berkeley
raco pkg install --auto planet-dyoo-simply-scheme1
```

- then you can use whatever editor and using berkeley's scheme library is as easy as:
```scheme
(require berkeley)
```

### Emacs
- If you'd like to do the full immersion and start on emacs as well, I can recommend [spacemacs](https://www.spacemacs.org/)
- It's a bit bloated but eases up the transitioning (especially if you're used to vim as I am)
- The configuration with evil-mode works quite well out of the box, and setting up racket and guile is as easy as enabling the respective layers:
  - https://develop.spacemacs.org/layers/+lang/scheme/README.html
  - https://develop.spacemacs.org/layers/+lang/racket/README.html
- Since racket uses `.rkt` extension (instead of the standard `.scm`), emacs will switch the major mode between them seamlessly

## Structure
- CS61A skips some exercises and also switches the orders from the first chapter
- Because of that I've split the course and book materials
- all exercises from the book (asked or not in CS61A), are inside `book`
- exercises from the course (homeworks / projects / midterms) are in CS61A
