#lang info
(define collection "expect")
(define scribblings
  '(("scribblings/main.scrbl" (multi-page) ("Testing") "expect")))
(define deps
  '("syntax-classes-lib"
    ("arguments" #:version "1.1")
    "base"
    "fancy-app"
    ("rackunit-lib" #:version "1.7")
    "reprovide-lang"))
(define build-deps
  '("rackunit-doc"
    "doc-coverage"
    "racket-doc"
    "scribble-lib"
    "scribble-text-lib"
    "syntax-classes-lib"))
