#lang scribble/manual

@(require "base.rkt")

@title{Expectation Constructors}

The @racketmodname[expect] library provides several built-in
@expectation-tech{expectations} and expectation constructors. The provided
expectations are intended to supplant all the functionality of the checks
provided by @racketmodname[rackunit].

@section{Equality Expectations}

@defproc[(expect-eq? [v any/c]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is @racket[eq?]
 to @racket[v], returning a single @fault-tech{fault} otherwise. The fault
 contains an @racket[eq-attribute] and a @racket[self-attribute] for the
 expected and actual fields, respectively.
 @(expect-examples
   (eval:error (expect! 'bar (expect-eq? 'foo))))}

@defproc[(expect-eqv? [v any/c]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is @racket[eqv?]
 to @racket[v], returning a single @fault-tech{fault} otherwise. The fault
 contains an @racket[eqv-attribute] and a @racket[self-attribute] for the
 expected and actual fields, respectively.
 @(expect-examples
   (eval:error (expect! 'bar (expect-eqv? 'foo))))}

@defproc[(expect-equal? [v any/c]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is
 @racket[equal?] to @racket[v]. Due to the recursive properties of
 @racket[equal?], the expectation may return multiple faults identifying
 specific sub-values that fail to meet the expectation. Most returned faults
 contain @racket[equal-attribute] and @racket[self-attribute] values for their
 expected and actual fields, respectively. However some sub-values may be of the
 wrong type or contain too many items, leading to faults with
 @racket[pred-attribute] or @racket[length-attribute]. See @racket[expect-list]
 and the other compound data expectation constructors for details on what faults
 may be returned for these types of values. 
 @(expect-examples
   (eval:error (expect! '(1 foo (bar 4 5 extra) blah 7)
                        (expect-equal? '(1 2 (3 4 5) 6 7)))))}

@deftogether[
 (@defproc[(eq-attribute? [v any/c]) boolean?]
   @defproc[(eqv-attribute? [v any/c]) boolean?]
   @defproc[(equal-attribute? [v any/c]) boolean?])]{
 Predicates for the @racket[eq-attribute], @racket[eqv-attribute], and
 @racket[equal-attribute] structures respectively. Used by @racket[expect-eq?],
 @racket[expect-eqv?], and @racket[expect-equal?].}

@deftogether[
 (@defproc[(eq-attribute [value any/c]) eq-attribute?]
   @defproc[(eqv-attribute [value any/c]) eqv-attribute?]
   @defproc[(equal-attribute [value any/c]) equal-attribute?])]{
 Constructors for the @attribute-tech{attributes} used in @fault-tech{faults}
 returned by @racket[eq-attribute], @racket[eqv-attribute], and
 @racket[equal-attribute] respectively. Equality attributes have descriptions
 of the form @racket["<eq-proc> to <value>"], such as @racket["equal? to 'foo"].
 @(expect-examples
   (eqv-attribute 'foo)
   (equal-attribute 12.5))}

@deftogether[
 (@defproc[(eq-attribute-value [eq-attr eq-attribute?]) any/c]
   @defproc[(eqv-attribute-value [eqv-attr eqv-attribute?]) any/c]
   @defproc[(equal-attribute-value [equal-attr equal-attribute?]) any/c])]{
 Accessors for the value originally used to construct the respective attribute.
@(expect-examples
  (eq-attribute-value (eq-attribute 12)))}

@deftogether[
 (@defproc[(expect-not-eq? [v any/c]) expectation?]
   @defproc[(expect-not-eqv? [v any/c]) expectation?]
   @defproc[(expect-not-equal? [v any/c]) expectation?])]{
 Negated variants of @racket[expect-eq?], @racket[expect-eqv?], and
 @racket[expect-equal?] respectively. @fault-tech{Faults} returned by these
 expectations wrap their expected @attribute-tech{attribute} in
 @racket[not-attribute].
 @(expect-examples
   (eval:error (expect! '(1 foo) (expect-not-equal? '(1 2)))))}

@deftogether[
 (@defproc[(not-attribute [attr attribute?]) not-attribute?]
   @defproc[(not-attribute? [v any/c]) boolean?]
   @defproc[(not-attribute-negated [not-attr not-attribute?]) attribute?])]{
 Constructor, predicate, and field accessor for the @attribute-tech{attribute}
 returned by @racket[expect-not-eq?], @racket[expect-not-eqv?], and
 @racket[expect-not-equal?].
 @(expect-examples
   (define not-foo (not-attribute (eq-attribute 'foo)))
   not-foo
   (not-attribute? not-foo)
   (not-attribute-negated not-foo))}

@defproc[(expect-= [x real?] [epsilon real?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a number
 within @racket[epsilon] of @racket[x]. Returned @fault-tech{faults} have
 instances of @racket[=-attribute] in their expected field.
 @(expect-examples
   (define exp10 (expect-= 10 0.01))
   (expect! 10 exp10)
   (eval:error (expect! 25 exp10))
   (expect! 10.0001 exp10))}

@deftogether[
 (@defproc[(=-attribute [x real?] [epsilon real?]) =-attribute?]
   @defproc[(=-attribute? [v any/c]) boolean?]
   @defproc[(=-attribute-value [=-attr =-attribute?]) real?]
   @defproc[(=-attribute-epsilon [=-attr =-attribute?]) real?])]{
 Constructor, predicate, and field accessors for the @attribute-tech{attribute}
 returned by @racket[expect-=].
 @(expect-examples
   (define =10 (=-attribute 10 0.01))
   =10
   (=-attribute? =10)
   (=-attribute-value =10)
   (=-attribute-epsilon =10))}

@section{Boolean Expectations}

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
