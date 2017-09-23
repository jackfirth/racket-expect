#lang racket/base

(require expect
         expect/rackunit
         racket/list
         (only-in rackunit test-case))

(define-logger foo)

(define (log) (log-debug "debug message"))
(define (log/foo) (log-foo-debug "debug message"))
(define (raise-foo) (raise 'foo))

(define (expect-self exp)
  (expect-struct self-attribute [self-attribute-value exp]))

(define (exp-log-msgs n)
  (apply expect-list (make-list n (expect-vector-length 4))))

(test-case "expect-log*"
  ;; The foo-logger has the module's current-logger as its parent, so we use a
  ;; new logger as the current logger to ensure the default logger used by
  ;; expect-log* doesn't receive the message logged by log/foo
  (parameterize ([current-logger (make-logger)])
    (test-subject #:subject (expect-log* (expect-pred empty?))
      (expect-exp-faults void)
      (expect-exp-faults log
                         (expect-fault #:actual (expect-self (exp-log-msgs 1))
                                       #:contexts (list the-log-context)))
      (expect-exp-faults log/foo)
      (expect-exp-faults raise-foo
                         (expect-fault #:actual (expect-self 'foo)
                                       #:contexts (list the-raise-context)))))
  (test-subject "#:logger"
    #:subject (expect-log* (expect-pred empty?) #:logger foo-logger)
    (expect-exp-faults void)
    (expect-exp-faults log)
    (expect-exp-faults log/foo
                       (expect-fault #:actual (expect-self (exp-log-msgs 1))
                                     #:contexts (list the-log-context)))))
