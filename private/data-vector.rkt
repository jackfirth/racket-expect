#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-vector (rest-> expectation? expectation?)]
  [expect-vector-ref (-> expectation? exact-nonnegative-integer? expectation?)]
  [expect-vector-count (-> expectation? expectation?)]))

(require fancy-app
         "base.rkt"
         "combinator.rkt"
         "compare.rkt"
         "data-collect.rkt"
         "logic.rkt"
         "util.rkt")

(module+ test
  (require racket/function
           rackunit))


(define (expect-vector-ref exp idx)
  (expect/context (expect/proc exp (vector-ref _ idx)) (index-context idx)))

(define expect-vector-count (expect/count _ vector-length))

(define (expect-vector . exps)
  (define (vec->items-exp vec)
    (apply expect-all
           (map/index expect-vector-ref (take/chop exps (vector->list vec)))))
  (expect-and (expect-pred vector?)
              (expect-all (expect-vector-count (expect-equal? (length exps)))
                          (expect/dependent vec->items-exp))))

(module+ test
  (define expect-num+sym
    (expect-vector (expect-pred number?) (expect-pred symbol?)))
  (check-not-exn (thunk (expect! #(10 foo) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! #(10 20) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! #(foo bar) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! #(10) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! #(10 foo extra) expect-num+sym)))
  (check-exn exn:fail:expect? (thunk (expect! 'not-list expect-num+sym))))
