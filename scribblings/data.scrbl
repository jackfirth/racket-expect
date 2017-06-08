#lang scribble/manual

@(require "base.rkt")

@title{Data Structure Expectations}

@defproc[(expect-list [item-exp expectation-convertible?] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a list whose
 elements satisfy the @racket[item-exp] expectations. The length of the list is
 also checked, and only the @racket[item-exp] expectations for lists that
 contain enough items to include the corresponding @racket[item-exp] are
 checked.
 @(expect-examples
   (define num+string-expectation
     (expect-list (expect-pred number?) (expect-pred string?)))
   (expect! '(10 "text") num+string-expectation)
   (eval:error (expect! '(foo bar) num+string-expectation))
   (eval:error (expect! '(foo) num+string-expectation)))}

@defproc[(expect-list-ref [item-exp expectation-convertible?]
                          [index exact-nonnegative-integer?])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a list with
 an item at position @racket[index], then checks that item against
 @racket[item-exp].
 @(expect-examples
   (define expect-second-string? (expect-list-ref (expect-pred string?) 1))
   (expect! '(10 "text") expect-second-string?)
   (eval:error (expect! '(10 20) expect-second-string?)))}

@defproc[(expect-list-count [count-exp expectation-convertible?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a list, then
 checks the number of items in the list against @racket[count-exp].
 @(expect-examples
   (define expect-even-list (expect-list-count (expect-pred even?)))
   (expect! '(a b) expect-even-list)
   (eval:error (expect! '(a b c) expect-even-list)))}

@defproc[(expect-vector [item-exp expectation-convertible?] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a vector
 whose elements satisfy the @racket[item-exp] expectations. The length of the
 vector is also checked, and only the @racket[item-exp] expectations for vectors
 that contain enough items to include the corresponding @racket[item-exp] are
 checked.
 @(expect-examples
   (define num+foo-vec-expectation (expect-vector (expect-pred number?) 'foo))
   (expect! #(10 foo) num+foo-vec-expectation)
   (eval:error (expect! #(10 bar) num+foo-vec-expectation))
   (eval:error (expect! #(10) num+foo-vec-expectation)))}

@defproc[(expect-vector-ref [item-exp expectation-convertible?]
                            [index exact-nonnegative-integer?])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a vector with
 an item at position @racket[index], then checks that item against
 @racket[item-exp].
 @(expect-examples
   (define expect-second-string? (expect-vector-ref (expect-pred string?) 1))
   (expect! #(10 "text") expect-second-string?)
   (eval:error (expect! #(10 20) expect-second-string?)))}

@defproc[(expect-vector-count [count-exp expectation-convertible?])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a vector,
 then checks the number of items in the vector against @racket[count-exp].
 @(expect-examples
   (define expect-even-vector (expect-vector-count (expect-pred even?)))
   (expect! #(a b) expect-even-vector)
   (eval:error (expect! #(a b c) expect-even-vector)))}

@defproc[(expect-set [v any/c] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a set that
 contains exactly the given @racket[v] values and no other values. The
 expectation finds one @fault-tech{fault} for each extra item and for each
 missing item in the checked set.
 @(expect-examples
   (expect! (set 1 2 3) (expect-set 1 2 3))
   (eval:error (expect! (set 1 'foo) (expect-set 1 2 3))))}

@defproc[(expect-set-member? [v any/c]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a set
 containing @racket[v].
 @(expect-examples
   (expect! (set 1 2) (expect-set-member? 1))
   (eval:error (expect! (set 1 2) (expect-set-member? 'foo))))}

@defproc[(expect-set-not-member? [v any/c]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a set that
 does not contain @racket[v].
 @(expect-examples
   (expect! (set 1 2) (expect-set-not-member? 'foo))
   (eval:error (expect! (set 1 2) (expect-set-not-member? 1))))}

@defproc[(expect-subset [st set?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a set that
 is a subset of @racket[st]. The expectation finds one @fault-tech{fault} for
 each unexpected item.
 @(expect-examples
   (expect! (set 1 2) (expect-subset (set 1 2 3)))
   (eval:error (expect! (set 1 2 'foo 'bar) (expect-subset (set 1 2 3)))))}

@defproc[(expect-superset [st set?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a set that
 is a superset of @racket[st]. The expectation finds one @fault-tech{fault} for
 each item in @racket[st] not found in the checked set.
 @(expect-examples
   (expect! (set 1 2 3) (expect-superset (set 1 2)))
   (eval:error (expect! (set 'foo) (expect-superset (set 1 2)))))}

@defproc[(expect-set-count [count-exp expectation-convertible?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a set whose
 number of elements is then checked against @racket[count-exp]
 @(expect-examples
   (expect! (set 'foo 'bar) (expect-set-count 2))
   (eval:error (expect! (set 1 2 3) (expect-set-count (expect-pred even?)))))}
