#lang info
(define collection "expect")
(define scribblings
  '(("scribblings/main.scrbl" (multi-page) (library) "expect")))
(define deps
  '("base"
    "fancy-app"
    "reprovide-lang"))
(define build-deps
  '("rackunit-lib"
    "rackunit-doc"
    "doc-coverage"
    "racket-doc"
    "scribble-lib"
    "scribble-text-lib"))
