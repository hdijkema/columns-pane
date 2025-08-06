#lang scribble/manual

@(require
   scribble/example
   (for-label ;racket
              ;racket/base
              ;racket/string
              racket/gui
              ;racket/file
              )
  @(for-label columns-pane))

@title[#:tag "columns-pane"]{A pane to arrange controls in columns}

@author[@author+email["Hans Dijkema" "hans@dijkewijk.nl"]]

@defmodule[columns-pane]
This module provides a new class @racket[columns-pane%] that can be used to arrange widgets in a table like fashon, making all widgets in a column of the same width

@defclass[columns-pane% vertical-pane% ()]{

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
}

@defmethod*[([(column-min-width (c column-index) [w dimenstion-integer?]) dimension-integer?])]{
 Gets or sets the minimum width (in pixels) of the given column c.

 Returns the currently set minimum width for the given column. 
}

@defmethod*[([(column-align (c column-index) [align <- (or/c 'left 'center 'right)]) (or/c 'left 'center 'right) ])]{
  Gets or sets the horizontal alignment of a given column c.

  Returns the currently set alignment.

  Note. Use this right after declaring the columns pane and before adding any children. 
}

@defmethod*[([(column-keep-min-width (c column-index) [k (or/c #f #t)])]{
  Gets or sets the keep minimal width parameter for a given column c. 

  Returns the currently set value. 

  When this parameter is set for a column, the column-width will not shrink when the contents of the column get less width. 
  It will grow however, when needed. 
}


}

@section{Example code}

@#reader scribble/comment-reader 
[racketblock
   (require racket/gui)
   (require columns-pane)

   (define win (new frame% [label "A new frame"]))
   (define grid-group (new group-box-panel%
                           [label "A new group"] [parent win]
                           [horiz-margin 10] [vert-margin 10]))

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

@image{scribblings/columns-pane-example.png}

@section{Same panel without columns-pane}

@#reader scribble/comment-reader 
[racketblock
   (require racket/gui)

   (define win (new frame% [label "A new frame"]))
   (define grid-group (new group-box-panel%
                           [label "A new group"] [parent win]
                           [horiz-margin 10] [vert-margin 10]))

   ;; Adding the columns pane with 3 columns
   (define h1 (new horizontal-pane% [parent grid-group]))

   (define btn1 (new button% [parent h1] [label "Button 1"]))
   (define g1 (new gauge%  [parent h1] [stretchable-width #t] [label "gauge 1"] [range 100]))
   (define lbl1 (new message% [parent h1] [label "This is lbl 1"]))

   (define h2 (new horizontal-pane% [parent grid-group]))
   (define btn2 (new button% [parent h2] [label "Btn 2"]))
   (define lbl2 (new message% [parent h2] [label "This is lbl 2"]))

   ;; Now, when the window is shown, the columns pane will arrange it's children
   ;; to have the same width in each column. 
   (send win show #t)
]

@image{scribblings/without-columns-pane.png}
