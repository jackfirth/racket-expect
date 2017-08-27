#lang racket/base

(require racket/contract)

(module+ no-conversion
  (provide
   (contract-out
    [expect-list (rest-> expectation? expectation?)]
    [expect-list-ref (-> expectation? exact-nonnegative-integer? expectation?)]
    [expect-list-count (-> expectation? expectation?)])))

(require fancy-app
         "base.rkt"
         "combinator.rkt"
         (submod "compare.rkt" no-conversion)
         "data-collect.rkt"
         "logic.rkt"
         (submod "logic.rkt" no-conversion)
         "util.rkt")

(module+ test
  (require racket/function
           rackunit))


(define (expect-list-ref exp idx)
  (expect/context (expect/proc exp (list-ref _ idx)) (index-context idx)))

(define expect-list-count (expect/count _ length))

(define (expect-list . exps)
  (define (list->items-exp vs)
    (apply expect-all (map/index expect-list-ref (take/chop exps vs))))
  (expect-and (expect-pred list?)
              (expect-all (expect-list-count (expect-equal? (length exps)))
                          (expect/dependent list->items-exp))))

(module+ test
  (define expect-num+sym
    (expect-list (expect-pred number?) (expect-pred symbol?)))
  (check-not-exn (thunk (expect! '(10 foo) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! '(10 20) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! '(foo bar) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! '(10) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! '(10 foo extra) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! 'not-list expect-num+sym))))
