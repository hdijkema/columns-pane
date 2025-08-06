#lang scribble/manual

@(require
   scribble/example
   (for-label racket/base
              racket/string
              racket/gui
              racket/file))

(define myeval
   (make-base-eval '(require columns-pane)))

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[columns-pane]{This module provides a new class @racket[columns-pane%] that can be used to arrange widgets in a table like fashon, making all widgets in a column of the same width}

@defclass/title[columns-pane% vertical-pane% ()]{

A columns pane arranges its subwindows in columns. The number of columns must be given in advance and initializes to 1.
See also @racket[pane%].

@defconstructor[([parent (or/c (is-a?/c frame%) (is-a?/c dialog%) 
                               (is-a?/c panel%) (is-a?/c pane%))]
                 [columns 1]
                 [vert-margin spacing-integer? 0]
                 [horiz-margin spacing-integer? 0]
                 [border spacing-integer? 0]
                 [spacing spacing-integer? 0]
                 [alignment (list/c (or/c 'left 'center 'right)
                                    (or/c 'top 'center 'bottom))
                            '(left center)]
                 [min-width (or/c dimension-integer? #f) #f]
                 [min-height (or/c dimension-integer? #f) #f]
                 [stretchable-width any/c #t]
                 [stretchable-height any/c #t])]{

}}


@#reader scribble/comment-reader 
[racketblock
   (require racket/gui)
   (require columns-pane)

   (define win (new frame% [label "A new frame"]))
   (define grid-group (new group-box-panel%
                           [label "A new group"] [parent win] [horiz-margin 10] [vert-margin 10]))

   ;; Adding the columns pane with 3 columns
   (define g (new columns-pane% [parent grid-group] [columns 3]))

   (define btn1 (new button% [parent g] [label "Button 1"]))
   (define g1 (new gauge%  [parent g] [stretchable-width #t] [label "gauge 1"] [range 100]))
   (define lbl1 (new message% [parent g] [label "This is lbl 1"]))

   (define btn2 (new button% [parent g] [label "Btn 2"]))
   (define lbl2 (new message% [parent g] [label "This is lbl 2"]))

   ;; Now, when the window is shown, the columns pane will arrange it's children
   ;; to have the same width in each column. 
   (send win show #t)
]


