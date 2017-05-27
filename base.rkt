#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect! (-> expectation? any/c void?)]
  [expectation (-> (-> any/c (listof fault?)) expectation?)]
  [expectation? predicate/c]
  [expectation-apply (-> expectation? any/c result?)]
  [expectation-apply/faults (-> expectation? any/c (listof fault?))]
  [result (-> any/c (listof fault?) result?)]
  [result? predicate/c]
  [result-subject (-> result? any/c)]
  [result-faults (-> result? (listof fault?))]
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
  [struct (self-attribute attribute)
    ([description string?] [value any/c])]
  [make-self-attribute (-> any/c self-attribute?)]
  [struct context ([description string?]) #:omit-constructor]
  [struct attribute ([description string?]) #:omit-constructor]
  [struct (exn:fail:expect exn:fail)
    ([message string?]
     [continuation-marks continuation-mark-set?]
     [result result?])]))

(require racket/format
         racket/list
         racket/string)


(struct expectation (proc))
(struct result (subject faults) #:transparent)
(struct context (description) #:transparent)
(struct attribute (description) #:transparent)
(struct self-attribute attribute (value) #:transparent)

(define (make-self-attribute v) (self-attribute (~v v) v))

(struct fault (summary expected actual contexts)
  #:transparent #:omit-define-syntaxes #:constructor-name make-fault)

(define (fault #:summary summary
               #:expected expected
               #:actual actual
               #:contexts [contexts (list)])
  (make-fault summary expected actual contexts))

(define (expectation-apply/faults exp v) ((expectation-proc exp) v))
(define (expectation-apply exp v)
  (result v (expectation-apply/faults exp v)))

(struct exn:fail:expect exn:fail (result) #:transparent)

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

(define (result-message/singular rslt)
  (define flt (first (result-faults rslt)))
  (error-message (format "expected ~a" (fault-summary flt))
                 (list* (format "subject: ~v" (result-subject rslt))
                        (fault-messages flt))))

(define (result-message/plural rslt)
  (define (fault-message flt)
    (error-message (format "expected: ~a" (fault-summary flt))
                   (fault-messages flt)
                   #:indent-depth 1))
  (error-message "multiple failures"
                 (list* (format "subject: ~v" (result-subject rslt))
                        (map fault-message (result-faults rslt)))))

(define (raise-result rslt)
  (define num-faults (length (result-faults rslt)))
  (when (= num-faults 1)
    (raise (exn:fail:expect (result-message/singular rslt)
                            (current-continuation-marks)
                            (result-faults rslt))))
  (when (> num-faults 1)
    (raise (exn:fail:expect (result-message/plural rslt)
                            (current-continuation-marks)
                            (result-faults rslt)))))

(define (expect! exp v)
  (raise-result (expectation-apply exp v)))
