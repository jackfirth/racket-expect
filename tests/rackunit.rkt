#lang racket/base

(module+ test
  (require arguments
           expect
           expect/rackunit
           (only-in rackunit test-case)
           "util.rkt")

  (test-case "check-expect"
    (check-expect #t expect-true)
    (check-fail check-expect (arguments #t expect-false))
    (check-fail check-expect (arguments '(a 2) (expect-equal? '(1 2))))
    (check-fail check-expect (arguments '(a b) (expect-equal? '(1 2)))))

  (test-case "equality checks"
    (check-eq? 1 1)
    (check-fail check-eq? (arguments 1 2))
    (check-eqv? 1 1)
    (check-fail check-eqv? (arguments 1 2))
    (check-equal? 1 1)
    (check-fail check-equal? (arguments 1 2)))

  (test-case "negated equality checks"
    (check-not-eq? 1 2)
    (check-fail check-not-eq? (arguments 1 1))
    (check-not-eqv? 1 2)
    (check-fail check-not-eqv? (arguments 1 1))
    (check-not-equal? 1 2)
    (check-fail check-not-equal? (arguments 1 1)))

  (test-case "pred / comparison checks"
    (check-pred number? 1)
    (check-fail check-pred (arguments number? 'foo))
    (check-= 1 1.1 0.5)
    (check-fail check-= (arguments 1 2 0.5))
    (check < 4 10)
    (check-fail check (arguments > 4 10)))

  (test-case "boolean checks"
    (check-true #t)
    (check-fail check-true (arguments #f))
    (check-fail check-true (arguments 'foo))
    (check-false #f)
    (check-fail check-false (arguments #t))
    (check-fail check-false (arguments 'foo))
    (check-not-false #t)
    (check-not-false 'foo)
    (check-fail check-not-false (arguments #f)))

  (test-case "exn checks"
    (define (raise-foo) (raise 'foo))
    (check-exn symbol? raise-foo)
    (check-fail check-exn (arguments number? raise-foo))
    (check-fail check-exn (arguments symbol? void))
    (check-not-exn void)
    (check-fail check-not-exn (arguments raise-foo))))
