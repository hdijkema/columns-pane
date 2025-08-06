#lang info

(define pkg-authors '(hnmdijkema))
(define version "0.1.2")
(define license 'Apache-2.0)
(define collection "columns-pane")
(define pkg-desc "A pane that arranges it's children widgets in columns")

(define scribblings
  '(
    ("scribblings/columns-pane.scrbl" () (library) "columns-pane")
    )
  )

(define deps
  '("racket/gui" "racket/base" "racket/core"))

(define build-deps
  '("racket-doc"
    "rackunit-lib"
    "scribble-lib"))

