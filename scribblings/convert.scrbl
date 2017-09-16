#lang scribble/manual

@(require "base.rkt")

@title{Conversion to Expectations}

@defproc[(->expectation [v any/c]) expectation?]{
 Returns an @expectation-tech{expectation} constructed by converting @racket[v]
 to an expectation. Expectation conversion occurs via the following process:

 @(itemlist
   @item{Any expectation (according to @racket[expectation?]) is convertible to
  itself.}
   @item{Lists are convertible with @racket[expect-list] after first converting
  their contained items.}
   @item{Hashes (but not generic dictionaries) are convertible with
  @racket[expect-hash] after first converting their contained values.}
   @item{Vectors are convertible with @racket[expect-vector] after first
  converting their contained items.}
   @item{Sets are convertible with @racket[expect-set]. Items in the set are
  not converted, as that would have no sensible definition that respected the
  properties of sets.}
   @item{Syntax objects are convertible with @racket[expect-syntax] after first
  converting the syntax object's contents.}
   @item{All other values are convertible to expectations constructed with
  @racket[expect-equal?].})

 This process roughly means that @racket[v] is converted to an expectation that
 checks that its input is @racket[equal?] to @racket[v], @bold{unless @racket[v]
  is a container with expectations inside it}. For example, note the difference
 between the following two expectations:

 @(expect-examples
   (expect! (list 1 2) (->expectation (list 1 expect-any)))
   (eval:error (expect! (list 1 2) (expect-equal? (list 1 expect-any)))))

 So @racket[->expectation] can be thought of a variant of @racket[expect-equal?]
 that allows specifying that sub-structures of the value should match some
 expectation instead of merely being @racket[equal?] to an expected value.

 @bold{WARNING:} Not all built-in Racket collection types are supported, and
 there is no way for custom data types to cooperate with @racket[->expectation].
 These limitations may be addressed by future versions of this library.}
