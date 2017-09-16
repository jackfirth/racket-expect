#lang racket/base

;; This is a lightweight reimplementation of expect-apply with the following
;; restrictions:
;;
;;   - Only return value expectations are allowed
;;   - Only single value input arguments are allowed
;;   - Only single return value procedures are allowed
;;
;; This is so the data structure expectations can provide the same fault
;; contexts that an equivalent use of expect-apply would produce without
;; causing a cyclic dependency between expect/private/function and
;; expect/private/data. This is used for procedures like expect-hash-keys and
;; expect-list-count, and is not meant for use outside expect/private/data.

(provide expect-apply1
         make-apply1-context)

(require expect/private/lite
         expect/private/function-context
         "context.rkt")


(define (make-apply1-context f)
  (define ctxts
    (list (make-apply-context f) the-return-context (make-sequence-context 1)))
  (define desc (format "the return value of ~a" (object-name f)))
  (make-splice-context ctxts #:description desc))

(define (expect-apply1 f ret-exp)
  (define ctxt (make-apply1-context f))
  (expect/context (expect/proc ret-exp f) ctxt))
