#lang racket

(require racket/contract)

(provide
 (contract-out
  [expect! (-> any/c any/c void?)]
  [struct (exn:fail:expect exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [subject any/c]
     [faults (listof fault?)])]))

(require "base.rkt"
         "data.rkt")

(module+ test
  (require rackunit))


(struct exn:fail:expect exn:fail (subject faults) #:transparent)

(define (string-indent str indent-str)
  (define replace-str (string-append "\n" indent-str))
  (string-append indent-str (string-replace str "\n" replace-str)))

(define (context-message ctxt)
  (format "in: ~a" (context-description ctxt)))

(define (expected-message attr)
  (format "expected: ~a" (attribute-description attr)))

(define (actual-message attr)
  (format "actual: ~a" (attribute-description attr)))

(define (lines-join lines) (string-join lines "\n"))

(define (error-message summary notes #:indent-depth [depth 2])
  (define indent-str (make-string depth #\space))
  (lines-join (list summary (string-indent (lines-join notes) indent-str))))

(define (fault-messages flt)
  (append (map context-message (fault-contexts flt))
          (list (expected-message (fault-expected flt))
                (actual-message (fault-actual flt)))))

(module+ test
  (struct test-context context () #:transparent)
  (define flt/ctxts
    (fault #:summary "foo"
           #:expected (make-self-attribute 'foo)
           #:actual (make-self-attribute 'bar)
           #:contexts (list (test-context "outer") (test-context "inner"))))
  (check-equal? (fault-messages flt/ctxts)
                (list "in: outer" "in: inner" "expected: 'foo" "actual: 'bar")))

(define (result-message/singular subject flt)
  (error-message (format "expected ~a" (fault-summary flt))
                 (list* (format "subject: ~v" subject)
                        (fault-messages flt))))

(define (result-message/plural subject flts)
  (define (fault-message flt)
    (error-message (format "fault: expected ~a" (fault-summary flt))
                   (fault-messages flt)
                   #:indent-depth 1))
  (error-message "multiple failures"
                 (list* (format "subject: ~v" subject)
                        (map fault-message flts))))

(define (raise-result subject flts)
  (define num-faults (length flts))
  (when (= num-faults 1)
    (raise (exn:fail:expect (result-message/singular subject (first flts))
                            (current-continuation-marks)
                            subject
                            flts)))
  (when (> num-faults 1)
    (raise (exn:fail:expect (result-message/plural subject flts)
                            (current-continuation-marks)
                            subject
                            flts))))

(define (expect! v exp)
  (raise-result v (expectation-apply (->expectation exp) v)))

(module+ test
  (check-not-exn (thunk (expect! 'any expect-any)))
  (check-not-exn (thunk (expect! 1 1)))
  (check-exn exn:fail:expect? (thunk (expect! 1 2)))
  (define foo-fault (fault #:summary "foo"
                           #:expected (make-self-attribute 'foo)
                           #:actual (make-self-attribute 'not-foo)))
  (define expect-foo (expectation (const (list foo-fault))))
  (define (expect-foo!) (expect! 'any expect-foo))
  (check-exn exn:fail:expect? expect-foo!)
  (check-exn #rx"expected foo" expect-foo!)
  (check-exn #rx"expected: 'foo" expect-foo!)
  (check-exn #rx"actual: 'not-foo" expect-foo!)
  (define bar-fault (fault #:summary "bar"
                           #:expected (make-self-attribute 'bar)
                           #:actual (make-self-attribute 'not-bar)))
  (define expect-foo+bar (expectation (const (list foo-fault bar-fault))))
  (define (expect-foo+bar!) (expect! 'any expect-foo+bar))
  (check-exn #rx"multiple failures" expect-foo+bar!)
  (check-exn #rx"fault: expected foo" expect-foo+bar!)
  (check-exn #rx"fault: expected bar" expect-foo+bar!))
