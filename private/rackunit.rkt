#lang racket/base

(provide check-expect)

(require fancy-app
         racket/function
         racket/list
         racket/match
         racket/contract
         rackunit
         rackunit/log
         "base.rkt"
         "convert-base.rkt")

(module+ for-custom-checks
  (provide check-expect*))

(define-check (check-expect v exp) (check-expect* v exp))

(define (check-expect* v exp)
  (match (expectation-apply (expectation-convert exp) v)
    [(list) (void)]
    [(list flt) (fail-check-expect/singular v flt)]
    [(list* flts) (fail-check-expect/plural v flts)]))

(define (fail-check-expect/singular v flt)
  (define infos (fault-infos flt))
  (define infos/subject
    (if (member 'context (map check-info-name infos))
        (cons (make-check-info 'subject v) infos)
        infos))
  (with-check-info* infos/subject
    (thunk (fail-check (format "Expected ~a" (fault-summary flt))))))

(define (fail-check-expect/plural v flts)
  (with-check-info* (cons (make-check-actual v)
                          (map fault-infos-nested flts))
    (thunk (fail-check "Multiple faults found"))))

(define (fault-infos flt)
  (list/if (actual-info flt) (expected-info flt) (contexts-info flt)))

(define/contract (fault-infos-nested flt)
  (-> fault? check-info?)
  (define nested-infos (cons (summary-info flt) (fault-infos flt)))
  (make-check-info (string->uninterned-symbol "fault")
                   (nested-info nested-infos)))

(define (summary-info flt)
  (make-check-info 'summary
                   (string-info (format "expected ~a" (fault-summary flt)))))

(define (actual-info flt)
  (make-check-info 'actual
                   (string-info (attribute-description (fault-actual flt)))))

(define (expected-info flt)
  (make-check-info 'expected
                   (string-info (attribute-description (fault-expected flt)))))

(define (contexts-info flt)
  (define ctxts (fault-contexts flt))
  (and (not (empty? ctxts))
       (make-check-info 'context (nested-info (map context-info ctxts)))))

(define (context-info ctxt)
  (make-check-info 'in (string-info (context-description ctxt))))

(define (list/if . vs) (filter values vs))
