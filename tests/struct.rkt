#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/struct-info)
         expect
         expect/rackunit
         racket/function
         (only-in rackunit test-case))

(struct expand-context context () #:transparent)
(define the-expand-context (expand-context "call to expand syntax"))

(define (expect-expand exp)
  (define ((expand-thnk stx)) (expand stx))
  (expect/context (expect/proc exp expand-thnk) the-expand-context))

(define expect-syntax-exn
  (expect-expand (expect-raise (expect-struct exn:fail:syntax))))

(struct fish (color weight))
(define red-fish-passes (expect-exp-no-faults (fish 'red 5)))

(test-case "expect-struct"
  (check-expect (expect-struct fish [fish-color 'red] [fish-weight 5])
                red-fish-passes)
  (test-case "accessor-order"
    (check-expect (expect-struct fish [fish-weight 5] [fish-color 'red])
                  red-fish-passes))
  (test-case "accessor-optional"
    (check-expect (expect-struct fish [fish-color 'red]) red-fish-passes))
  (test-case "predicate"
    (check-expect (expect-struct fish) (expect-exp-one-fault 5)))
  (test-case "syntax"
    (test-case "accessor-unbound"
      (check-expect #'(let ()
                        (struct fish (color weight))
                        (expect-struct fish [unbound 4]))
                    expect-syntax-exn))
    (test-case "non-accessor-binding"
      (check-expect #'(let ()
                        (define legs #f)
                        (expect-struct fish [legs 4]))
                    expect-syntax-exn))
    (test-case "duplicate"
      (check-expect #'(let ()
                        (struct fish (color weight))
                        (expect-struct fish
                                       [fish-color 'red] [fish-color 'red]))
                    expect-syntax-exn))
    (test-case "predicate-unknown"
      (check-expect #'(let ()
                        (struct fish (color weight))
                        (define-syntax fish/no-pred
                          (list-set (extract-struct-info
                                     (syntax-local-value #'fish))
                                    2 #f))
                        (expect-struct fish/no-pred))
                    expect-syntax-exn))
    (test-case "predicate-unbound"
      (check-expect #'(let ()
                        (struct fish (color weight))
                        (define-syntax fish/bad-pred
                          (list-set (extract-struct-info
                                     (syntax-local-value #'fish))
                                    2 #'unbound))
                        (expect-struct fish/bad-pred))
                    expect-syntax-exn))
    (test-case "parent-field-disallowed"
      (define stx
        #'(let ()
            (struct parent (a))
            (struct child (b))
            (expect-struct child [parent-a 'foo])))
      (check-expect stx expect-syntax-exn))))

(test-case "define-struct-expectation"
  (define-struct-expectation fish)
  (check-expect (expect-fish #:color 'red) red-fish-passes)
  (test-case "alternate-name"
    (define-struct-expectation (expfish fish))
    (check-expect (expfish #:color 'red) red-fish-passes))
  (test-case "keyword-ambiguous"
    (define stx
      #'(let ()
          (struct fish (color weight))
          (define-syntax fish/ambiguous-kw-accessor
            (list-set (extract-struct-info
                       (syntax-local-value #'fish))
                      3 (list #'fish-color #'get-weight)))
          (define-struct-expectation fish/ambiguous-kw-accessor)))
    (check-expect stx expect-syntax-exn)))
