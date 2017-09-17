#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [expect-set-member? (-> any/c expectation?)]
  [expect-set-not-member? (-> any/c expectation?)]
  [expect-subset (-> set? expectation?)]
  [expect-superset (-> set? expectation?)]
  [expect-set (rest-> any/c expectation?)]
  [the-set-count-context splice-context?]))

(module+ for-conversion
  (provide
   (contract-out
    [expect-set-count (-> expectation? expectation?)])))

(require fancy-app
         racket/set
         expect/private/lite
         expect/private/util
         "kernel-apply.rkt")


(define the-set-count-context (make-apply1-context set-count))

(define (expect-set-member? v)
  (expect-and (expect-pred set?) (expect-contains? set-member? v)))

(define (expect-set-not-member? v)
  (expect-and (expect-pred set?) (expect-not-contains? set-member? v)))

(define (expect-subset big-st)
  (define exp (expect-and (expect-pred set?) (expect-subset* big-st)))
  (expectation-rename exp 'subset))

(define (expect-subset* big-st)
  (define (make-expectation-from-extras little-st)
    (define extras (set->list (set-subtract little-st big-st)))
    (expect-contains-none? set-member? extras))
  (expect/dependent make-expectation-from-extras))

(define (expect-superset little-st)
  (define exp (expect-and (expect-pred set?) (expect-superset* little-st)))
  (expectation-rename exp 'superset))

(define (expect-superset* little-st)
  (expect-contains-all? set-member? (set->list little-st)))

(define (expect-set-count exp)
  (define count-exp
    (expect-and (expect-pred set?) (expect-apply1 set-count exp)))
  (expectation-rename count-exp 'set-count))

(define (expect-set . vs)
  (define st (list->set vs))
  (define exp
    (expect-and (expect-pred set?)
                (expect-all (expect-superset* st) (expect-subset* st))))
  (expectation-rename exp 'set))
