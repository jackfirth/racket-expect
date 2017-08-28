#lang racket/base

(module+ test
  (require doc-coverage
           expect
           expect/rackunit)
  (check-all-documented 'expect)
  (check-all-documented 'expect/rackunit))
