#lang scribble/manual

@(require "base.rkt")

@title{Boolean and Logic Expectations}

@deftogether[
 (@defthing[expect-true expectation?]
   @defthing[expect-false expectation?]
   @defthing[expect-not-false expectation?])]{
 @expectation-tech{Expectations} that expect a value is either @racket[#t],
 @racket[#f], or not @racket[#f] respectively. Returned @fault-tech{faults} have
 @racket[self-attribute] values in the expected field, except for
 @racket[expect-not-false] which wraps a @racket[self-attribute] value in a
 @racket[not-attribute] value.
 @(expect-examples
   (eval:error (expect! 'foo expect-true))
   (eval:error (expect! 'foo expect-false))
   (expect! 'foo expect-not-false)
   (eval:error (expect! #f expect-not-false)))}

@defproc[(expect-pred [pred predicate/c]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value results in
 @racket[(pred v)] returning @racket[#t]. Returned @fault-tech{faults} have
 @racket[pred-attribute] values and @racket[self-attribute] values in their
 expected and actual fields respectively.
 @(expect-examples
   (expect! 10 (expect-pred number?))
   (eval:error (expect! 'foo (expect-pred number?))))}

@deftogether[
 (@defproc[(pred-attribute [pred predicate/c]) pred-attribute?]
   @defproc[(pred-attribute? [v any/c]) boolean?]
   @defproc[(pred-attribute-value [pred-attr pred-attribute?]) predicate/c])]{
 Constructor, predicate, and field accessor for the @attribute-tech{attribute}
 returned by @racket[expect-pred].
 @(expect-examples
   (define number-attr (pred-attribute number?))
   number-attr
   (pred-attribute? number-attr)
   (pred-attribute-value number-attr))}

@defproc[(expect-all [exp expectation?] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects everything that each of
 the given @racket[exp] values expects. All expectations are tested and each
 expectation's @fault-tech{faults} are returned together with all other faults.
 To combine expectations in a way where later expectations are tested only if
 earlier ones pass, see @racket[expect-and].
 @(expect-examples
   (define positive-even
     (expect-all (expect-pred positive?) (expect-pred even?)))
   (expect! 4 positive-even)
   (eval:error (expect! 7 positive-even))
   (eval:error (expect! -4 positive-even))
   (eval:error (expect! -7 positive-even)))}

@defproc[(expect-and [exp expectation?] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects everything that each of
 the given @racket[exp] values expects. Each expectation is tested in order and
 if any expectation finds any @fault-tech{faults}, those faults are returned
 immediately and the remaining @racket[exp] expectations are not tested. To
 combine expectations in a way where all expectations are tested and all faults
 returned together, see @racket[expect-all].
 @(expect-examples
   (define (small? x) (< x 10))
   (define small-number
     (expect-and (expect-pred real?)
                 (expect-pred positive?)
                 (expect-pred small?)))
   (expect! 5 small-number)
   (eval:error (expect! 20 small-number))
   (eval:error (expect! -4 small-number))
   (eval:error (expect! 'foo small-number)))}
