#lang racket/base

(module+ test
  (require doc-coverage
           expect)
  (check-all-documented 'expect))
