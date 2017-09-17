#lang scribble/manual

@(require "base.rkt")

@title{Comparison and Equality Expectations}

@defproc[(expect-compare [compare (-> any/c any/c any/c)] [other any/c])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value @var[v] that,
 when compared to @racket[other] by calling @racket[(compare v other)], returns
 a true value.

 @(expect-examples
   (define expect-foo (expect-compare string=? "foo"))
   (expect! "foo" expect-foo)
   (eval:error (expect! "bar" expect-foo)))}

@defproc[(expect-not-compare [compare (-> any/c any/c any/c)] [other any/c])
         expectation?]{
 Like @racket[expect-compare], but for an input @var[v] the returned expectation
 checks that @racket[(compare v other)] returns false instead of true.

 @(expect-examples
   (define expect-not-foo (expect-not-compare string=? "foo"))
   (expect! "bar" expect-not-foo)
   (eval:error (expect! "foo" expect-not-foo)))}

@defproc[(expect-contains? [contains? (-> any/c any/c any/c)] [v any/c])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a container value
 @var[c], that contains @racket[v] as determined by whether
 @racket[(contains? c v)] returns a true value.

 @(expect-examples
   (define exp-foo (expect-contains? hash-has-key? 'foo))
   (expect! (hash 'foo 1 'bar 2) exp-foo)
   (eval:error (expect! (hash 'bar 2) exp-foo)))}

@defproc[(expect-not-contains? [contains? (-> any/c any/c any/c)] [v any/c])
         expectation?]{
 Like @racket[expect-contains?], but for an input @var[c] the returned
 expectation checks that @racket[(contains? c v)] returns false instead of true.

 @(expect-examples
   (define exp-not-foo (expect-not-contains? hash-has-key? 'foo))
   (expect! (hash 'bar 2) exp-not-foo)
   (eval:error (expect! (hash 'foo 1 'bar 2) exp-not-foo)))}

@defproc[(expect-contains-all? [contains? (-> any/c any/c any/c)] [vs list?])
         expectation?]{
 Like @racket[expect-contains?], but for an input @var[c] the returned
 expectation checks that every item @var[v] in @racket[vs] is contained in
 @racket[c] according to @racket[(contains? c v)]. Only one @fault-tech{fault}
 is returned which has an @racket[and-attribute] value containing one
 @racket[contains-attribute] for each missing @racket[v]. See
 @racket[make-contains-all-attribute] for details on how this attribute is
 constructed.

 @(expect-examples
   (define exp-keys (expect-contains-all? hash-has-key? '(foo bar baz)))
   (expect! (hash 'foo 1 'bar 2 'baz 3) exp-keys)
   (eval:error (expect! (hash 'foo 1 'blah 4) exp-keys)))}

@defproc[(expect-contains-none? [contains? (-> any/c any/c any/c)] [vs list?])
         expectation?]{
 Like @racket[expect-contains?], but for an input @var[c] the returned
 expectation checks that no item @var[v] in @racket[vs] is contained in
 @racket[c] according to @racket[(contains? c v)]. Only one @fault-tech{fault}
 is returned which has a @racket[not-attribute] wrapping an
 @racket[or-attribute] containing one @racket[contains-attribute] for each
 present @racket[v]. See @racket[make-contains-none-attribute] for details on
 how this attribute is constructed.

 @(expect-examples
   (define exp-no-keys (expect-contains-none? hash-has-key? '(foo bar baz)))
   (expect! (hash 'blah 4) exp-no-keys)
   (eval:error (expect! (hash 'foo 1 'baz 3 'blah 4) exp-no-keys)))}

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
              ([proc (-> any/c any/c any/c)] [other any/c])
              #:transparent #:omit-constructor]
   @defproc[(make-compare-attribute [proc (-> any/c any/c any/c)] [other any/c])
            compare-attribute?])]{
 An @attribute-tech{attribute} and its constructor that represents the result of
 comparing the input value to @racket[other] using @racket[proc]. See
 @racket[expect-compare] for examples.}

@deftogether[
 (@defstruct*[(contains-attribute attribute)
              ([proc (-> any/c any/c any/c)] [value any/c])
              #:transparent #:omit-constructor]
   @defproc[(make-contains-attribute [proc (-> any/c any/c any/c)]
                                     [value any/c])
            contains-attribute?])]{
 An @attribute-tech{attribute} and its constructor that represnts whether the
 input value contains @racket[value] according to @racket[proc]. See
 @racket[expect-contains] for examples.}

@defproc[(make-contains-all-attribute [contains? (-> any/c any/c any/c)]
                                      [vs list?])
         and-attribute?]{
 Returns an @racket[and-attribute] value wrapping a list of
 @racket[contains-attribute] values, one for each of @racket[vs]. The
 description of the returned attribute is more succinct than the default
 description that @racket[and-attribute] would normally create. Used by
 @racket[expect-contains-all].

 @(expect-examples
   (make-contains-all-attribute hash-has-key? '(foo bar baz)))}

@defproc[(make-contains-none-attribute [contains? (-> any/c any/c any/c)]
                                       [vs list?])
         not-attribute?]{
 Returns a @racket[not-attribute] value wrapping an @racket[or-attribute]
 which contains a list of @racket[contains-attribute] values, one for each of
 @racket[vs]. The description of the returned attribute is more succinct than
 the default description that @racket[or-attribute] would normally create. Used
 by @racket[expect-contains-none].

 @(expect-examples
   (make-contains-none-attribute hash-has-key? '(foo bar baz)))}

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
