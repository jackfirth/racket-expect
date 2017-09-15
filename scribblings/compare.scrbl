#lang scribble/manual

@(require "base.rkt")

@title{Comparison and Equality Expectations}

@defproc[(expect-compare [compare (-> any/c any/c any/c)] [other any/c])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value @var[v] that,
 when compared to @racket[other] by calling @racket[(compare v other)], returns
 a true value.

 @(expect-examples
   (define expect-small (expect-compare < 10))
   (expect! 6 expect-small)
   (eval:error (expect! 7256 expect-small)))}

@defproc[(expect-not-compare [compare (-> any/c any/c any/c)] [other any/c])
         expectation?]{
 Like @racket[expect-compare], but for an input @var[v] the returned expectation
 checks that @racket[(compare v other)] returns false instead of true.

 @(expect-examples
   (define expect-not-small (expect-not-compare < 10))
   (expect! 7256 expect-not-small)
   (eval:error (expect! 6 expect-not-small)))}

@deftogether[
 (@defproc[(expect-eq? [v any/c]) expectation?]
   @defproc[(expect-eqv? [v any/c]) expectation?]
   @defproc[(expect-equal? [v any/c]) expectation?])]{
 Convenience shortands for calling @racket[expect-compare] with @racket[eq?],
 @racket[eqv?], or @racket[equal?], respectively. However, due to the recursive
 properties of @racket[equal?], expectations returned by  @racket[expect-equal?]
 will traverse and inspect all values contained in @racket[v] and the input to
 the expectation. This may result in more than one @fault-tech{fault} being
 returned, for instance @racket[expect-equal?] might find several faults
 pointing to specific incorrect values in a complex data structure instead of
 simply pointing out the actual data structure and expected data structure are
 not @racket[equal?].

 @(expect-examples
   (eval:error (expect! '(1 2 (foo 4 5) not-a-list 8)
                        (expect-equal? '(1 2 (3 4 5) (6 7) 8)))))}

@deftogether[
 (@defproc[(expect-not-eq? [v any/c]) expectation?]
   @defproc[(expect-not-eqv? [v any/c]) expectation?]
   @defproc[(expect-not-equal? [v any/c]) expectation?])]{
 Negated variants of @racket[expect-eq?], @racket[expect-eqv?], and
 @racket[expect-equal?] respectively.
 
 @(expect-examples
   (expect! '(1 foo) (expect-not-equal? '(1 2)))
   (eval:error (expect! 'a (expect-not-eq? 'a))))}

@defproc[(expect-= [x real?] [epsilon real?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a number
 within @racket[epsilon] of @racket[x]. Returned @fault-tech{faults} have
 instances of @racket[=-attribute] in their expected field.
 
 @(expect-examples
   (define exp10 (expect-= 10 0.01))
   (expect! 10 exp10)
   (eval:error (expect! 25 exp10))
   (expect! 10.0001 exp10))}

@section{Comparison Attributes}

@deftogether[
 (@defstruct*[(compare-attribute attribute)
              ([proc (-> any/c any/c any/c)] [other any/c]) #:omit-constructor]
   @defproc[(make-compare-attribute [proc (-> any/c any/c any/c)] [other any/c])
            compare-attribute?])]{
 An @attribute-tech{attribute} and its constructor that represents the result of
 comparing the input value to @racket[other] using @racket[proc]. See
 @racket[expect-compare] for examples.}

@deftogether[
 (@defstruct*[(=-attribute attribute) ([value real?] [epsilon real?])
              #:transparent #:omit-constructor]
   @defproc[(make-=-attribute [value real?] [epsilon real?]) =-attribute?])]{
 An @attribute-tech{attribute} and its constructor that represents the result of
 comparing the input value to @racket[value] with @racket[=], with a tolerance
 of @racket[epsilon] allowed to account for floating point inaccuracies.}

@deftogether[
 (@defproc[(eq-attribute? [v any/c]) boolean?]
   @defproc[(eqv-attribute? [v any/c]) boolean?]
   @defproc[(equal-attribute? [v any/c]) boolean?])]{
 Convenient shorthand predicates for identifying @racket[compare-attribute]
 values returned by @racket[make-eq-attribute], @racket[make-eqv-attribute], and
 @racket[make-equal-attribute], respectively.}

@deftogether[
 (@defproc[(make-eq-attribute [value any/c]) eq-attribute?]
   @defproc[(make-eqv-attribute [value any/c]) eqv-attribute?]
   @defproc[(make-equal-attribute [value any/c]) equal-attribute?])]{
 Convenient shorthands for calling @racket[make-compare-attribute] with
 @racket[eq?], @racket[eqv?], and @racket[equal?], respectively.}
