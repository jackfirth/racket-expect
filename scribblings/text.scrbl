#lang scribble/manual

@(require "base.rkt")

@title{Text and String Expectations}

@defproc[(expect-regexp-match
          [pattern regexp?]
          [result-exp (or/c (listof (or/c string? bytes? #f expectation?))
                            expectation?)
           expect-not-false])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is either a
 string, a bytestring, a path, or a port. Then, the value is matched against
 @racket[pattern] using @racket[regexp-match] and the match result is checked
 with @racket[result-exp]. Using the default for @racket[result-exp] checks
 that @racket[pattern] matches the input value and ignores the result of the
 match. If @racket[result-exp] is not an expectation, it is converted to one
 with @racket[->expectation].

 @(expect-examples
   (expect! "This is some message" (expect-regexp-match #rx"some"))
   (expect! "12x4x6" (expect-regexp-match #rx"x." '("x4")))
   (eval:error (expect! "12x4x6" (expect-regexp-match #rx"x." '("x6")))))}

@defproc[(expect-string-contains? [str string?]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a string that contains
 @racket[str]. Convenient shorthand for combining @racket[expect-contains] with
 @racket[expect-pred] and @racket[string?].

 @(expect-examples
   (expect! "This is some message" (expect-string-contains? "some message"))
   (eval:error
    (expect! "This is some message" (expect-string-contains? "foo"))))}

@section{String Attributes and Contexts}

@deftogether[
 (@defstruct*[(regexp-match-context context) ([regexp regexp?])
              #:transparent #:omit-constructor]
   @defproc[(make-regexp-match-context [regexp regexp?])
            regexp-match-context?])]{
 A @context-tech{context} and its constructor that indicates a
 @fault-tech{fault} occurred in the result of calling @racket[regexp-match] with
 @racket[regexp].}

@deftogether[
 (@defstruct*[(regexp-match-attribute attribute) ([regexp regexp?])
              #:transparent #:omit-constructor]
   @defproc[(make-regexp-match-attribute [regexp regexp?])
            regexp-match-attribute?])]{
 An @attribute-tech{attribute} and its constructor that refers to whether or not
 a value matches @racket[regexp].}
