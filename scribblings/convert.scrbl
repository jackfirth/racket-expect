#lang scribble/manual

@(require "base.rkt")

@title{Conversion to Expectations}

@defproc[(expectation-convertible? [v any/c]) boolean?]{
 Returns @racket[#t] when @racket[v] is a value that can be converted to an
 @expectation-tech{expectation}, returns @racket[#f] otherwise. Several kinds of
 values are convertible:

 @itemlist[
 @item{Any expectation (according to @racket[expectation?]) is convertible to
   itself.}
 @item{Booleans are convertible to either @racket[expect-true] or
   @racket[expect-false].}
 @item{Numbers, strings, symbols, and characters are convertible to expectations
   constructed with @racket[expect-equal?].}
 @item{Lists are convertible with @racket[expect-list] after first converting
   their contents.}
 @item{Vectors are convertible with @racket[expect-vector] after first
   converting their contents.}
 @item{Sets are convertible with @racket[expect-set].}]}

@defproc[(expectation-convert [v expectation-convertible?]) expectation?]{
 Returns an @expectation-tech{expectation} constructed by converting @racket[v]
 to an expectation. See @racket[expectation-convertible?] for a description of
 what values are and aren't convertible.}
