#lang info

(define pkg-authors '(hnmdijkema))
(define version "0.1.3")
(define license 'Apache-2.0)
(define collection "columns-pane")
(define pkg-desc "A pane that arranges it's children widgets in columns")

(define scribblings
  '(
    ("scribblings/columns-pane.scrbl" () (gui) "columns-pane")
    )
  )

(define deps
  '("racket/gui" "racket/base" "racket"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))

