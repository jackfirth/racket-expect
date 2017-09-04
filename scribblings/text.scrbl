#lang scribble/manual

@(require "base.rkt")

@title{Text and String Expectations}

@defproc[(expect-regexp-match
          [pattern regexp?]
          [result-exp (or/c (listof (or/c string? bytes? #f expectation?))
                            #f
                            expectation?)
           expect-not-false])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is either a
 string, a bytestring, a path, or a port. Then, the value is matched against
 @racket[pattern] using @racket[regexp-match] and the match result is checked
 with @racket[result-exp]. Using the default for @racket[result-exp] checks
 that @racket[pattern] matches the input value and ignores the result of the
 match. If @racket[result-exp] is not an expectation, it is converted to one
 with @racket[->expectation]. All @fault-tech{faults} returned by
 @racket[result-exp] have a @racket[regexp-match-context] value added to their
 @context-tech{contexts}.

 @(expect-examples
   (expect! "This is some message" (expect-regexp-match #rx"some"))
   (expect! "12x4x6" (expect-regexp-match #rx"x." '("x4")))
   (eval:error (expect! "12x4x6" (expect-regexp-match #rx"x." '("x6")))))}


@defstruct*[(regexp-match-context context) ([regexp regexp?])
            #:transparent #:omit-constructor]{
 A @context-tech{context} that indicates a @fault-tech{fault} occurred in the
 result of calling @racket[regexp-match] with @racket[regexp].}
