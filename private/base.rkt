#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect! (-> any/c expectation? void?)]
  [expectation (-> (-> any/c (listof fault?)) expectation?)]
  [expectation? predicate/c]
  [expectation-apply (-> expectation? any/c (listof fault?))]
  [fault (->* (#:summary string?
               #:actual attribute?
               #:expected attribute?)
              (#:contexts (listof context?))
              fault?)]
  [fault? predicate/c]
  [fault-summary (-> fault? string?)]
  [fault-actual (-> fault? attribute?)]
  [fault-expected (-> fault? attribute?)]
  [fault-contexts (-> fault? (listof context?))]
  [struct context ([description string?]) #:omit-constructor]
  [struct attribute ([description string?]) #:omit-constructor]
  [self-attribute (-> any/c self-attribute?)]
  [self-attribute? predicate/c]
  [self-attribute-value (-> self-attribute? any/c)]
  [struct (exn:fail:expect exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [subject any/c]
     [faults (listof fault?)])]))

(require racket/format
         racket/list
         racket/string)

(module+ test
  (require racket/function
           rackunit))


(struct expectation (proc))
(struct context (description) #:transparent)
(struct attribute (description) #:transparent)

(struct self-attribute attribute (value)
  #:transparent
  #:omit-define-syntaxes
  #:constructor-name make-self-attribute)

(define (self-attribute v) (make-self-attribute (~v v) v))

(struct fault (summary expected actual contexts)
  #:transparent #:omit-define-syntaxes #:constructor-name make-fault)

(define (fault #:summary summary
               #:expected expected
               #:actual actual
               #:contexts [contexts (list)])
  (make-fault summary expected actual contexts))

(define (expectation-apply exp v) ((expectation-proc exp) v))

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
  (append (map context-message (reverse (fault-contexts flt)))
          (list (expected-message (fault-expected flt))
                (actual-message (fault-actual flt)))))

(define (result-message/singular subject flt)
  (error-message (format "expected ~a" (fault-summary flt))
                 (list* (format "subject: ~v" subject)
                        (fault-messages flt))))

(define (result-message/plural subject flts)
  (define (fault-message flt)
    (error-message (format "fault: ~a" (fault-summary flt))
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
  (raise-result v (expectation-apply exp v)))

(module+ test
  (check-not-exn (thunk (expect! 'any (expectation (const (list))))))
  (define foo-fault (fault #:summary "foo"
                           #:expected (self-attribute 'foo)
                           #:actual (self-attribute 'not-foo)))
  (define expect-foo (expectation (const (list foo-fault))))
  (define (expect-foo!) (expect! 'any expect-foo))
  (check-exn exn:fail:expect? expect-foo!)
  (check-exn #rx"expected foo" expect-foo!)
  (check-exn #rx"expected: 'foo" expect-foo!)
  (check-exn #rx"actual: 'not-foo" expect-foo!))
