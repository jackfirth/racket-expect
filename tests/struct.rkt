#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/struct-info)
         expect
         expect/rackunit
         racket/function
         (only-in rackunit test-case))

;; test structs and definitions
(struct fish (color weight))
(struct shark fish (teeth))

(define legs #f)

(define-syntax fish/pred-unknown
  (list-set (extract-struct-info
             (syntax-local-value #'fish))
            2 #f))

(define-syntax fish/pred-undefined
  (list-set (extract-struct-info
             (syntax-local-value #'fish))
            2 #'unbound))

(define-syntax fish/ambiguous-kw-accessor
  (list-set (extract-struct-info
             (syntax-local-value #'fish))
            3 (list #'fish-color #'get-weight)))

;; namespace used by syntax expectations
(define-namespace-anchor here)
(define here-ns (namespace-anchor->namespace here))

;; shorthands
(define red-fish-passes (expect-exp-faults (fish 'red 5)))
(define (expect-syntax-exn/here msg-exp)
  (expect-syntax-exn msg-exp #:namespace here-ns))

(test-case "expect-struct"
  (check-expect (expect-struct fish [fish-color 'red] [fish-weight 5])
                red-fish-passes)
  (test-case "accessor-order"
    (check-expect (expect-struct fish [fish-weight 5] [fish-color 'red])
                  red-fish-passes))
  (test-case "accessor-optional"
    (check-expect (expect-struct fish [fish-color 'red]) red-fish-passes))
  (test-case "predicate"
    (check-expect (expect-struct fish) (expect-exp-faults 5 expect-any)))
  (test-case "subtype"
    (test-case "subtype-field"
      (check-expect (shark 'red 5 100) (expect-struct shark [shark-teeth 100])))
    (test-case "supertype-field"
      (check-expect (shark 'red 5 100) (expect-struct shark [fish-color 'red]))))
  (test-case "syntax"
    (test-case "accessor-undefined"
      (check-expect #'(expect-struct fish [unbound 4])
                    (expect-syntax-exn/here #rx"accessor.*undefined")))
    (test-case "non-accessor-binding"
      (check-expect #'(expect-struct fish [legs 4])
                    (expect-syntax-exn/here
                     #rx"not known.*accessor")))
    (test-case "duplicate"
      (check-expect #'(expect-struct fish [fish-color 'red] [fish-color 'red])
                    (expect-syntax-exn/here #rx"duplicate")))
    (test-case "predicate-unknown"
      (check-expect #'(expect-struct fish/pred-unknown)
                    (expect-syntax-exn/here #rx"predicate.*not known")))
    (test-case "predicate-undefined"
      (check-expect #'(expect-struct fish/pred-undefined)
                    (expect-syntax-exn/here #rx"predicate.*undefined")))))

(test-case "define-struct-expectation"
  (define-struct-expectation fish)
  (check-expect (expect-fish #:color 'red) red-fish-passes)
  (test-case "alternate-name"
    (define-struct-expectation (expfish fish))
    (check-expect (expfish #:color 'red) red-fish-passes))
  (test-case "keyword-ambiguous"
    (check-expect #'(define-struct-expectation fish/ambiguous-kw-accessor)
                  (expect-syntax-exn/here #rx"get-weight.*ambiguous keyword"))))
