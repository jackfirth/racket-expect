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
