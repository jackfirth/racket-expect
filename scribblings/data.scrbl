#lang scribble/manual

@(require "base.rkt")

@title{Data Structure Expectations}

@defproc[(expect-list [item-exp any/c] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a list whose
 elements satisfy the @racket[item-exp] expectations. Each @racket[item-exp] is
 converted to an expectation with @racket[->expectation]. The length of the list
 is also checked, and only the @racket[item-exp] expectations for lists that
 contain enough items to include the corresponding @racket[item-exp] are
 checked.
 @(expect-examples
   (define num+string-expectation
     (expect-list (expect-pred number?) (expect-pred string?)))
   (expect! '(10 "text") num+string-expectation)
   (eval:error (expect! '(foo bar) num+string-expectation))
   (eval:error (expect! '(foo) num+string-expectation)))}

@defproc[(expect-list-ref [item-exp any/c]
                          [index exact-nonnegative-integer?])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a list with
 an item at position @racket[index], then checks that item against
 @racket[item-exp]. The given @racket[item-exp] is converted to an expectation
 with @racket[->expectation].
 @(expect-examples
   (define expect-second-string? (expect-list-ref (expect-pred string?) 1))
   (expect! '(10 "text") expect-second-string?)
   (eval:error (expect! '(10 20) expect-second-string?)))}

@defproc[(expect-list-count
          [count-exp (or/c exact-nonnegative-integer? expectation?)])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a list, then
 checks the number of items in the list against @racket[count-exp]. If
 @racket[count-exp] is an integer, it is converted to an expectation with
 @racket[->expectation].
 @(expect-examples
   (define expect-even-list (expect-list-count (expect-pred even?)))
   (expect! '(a b) expect-even-list)
   (eval:error (expect! '(a b c) expect-even-list)))}

@defproc[(expect-vector [item-exp any/c] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a vector
 whose elements satisfy the @racket[item-exp] expectations. Each
 @racket[item-exp] is converted to an expectation with @racket[->expectation].
 The length of the vector is also checked, and only the @racket[item-exp]
 expectations for vectors that contain enough items to include the corresponding
 @racket[item-exp] are checked.
 @(expect-examples
   (define num+foo-vec-expectation (expect-vector (expect-pred number?) 'foo))
   (expect! #(10 foo) num+foo-vec-expectation)
   (eval:error (expect! #(10 bar) num+foo-vec-expectation))
   (eval:error (expect! #(10) num+foo-vec-expectation)))}

@defproc[(expect-vector-ref [item-exp any/c]
                            [index exact-nonnegative-integer?])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a vector with
 an item at position @racket[index], then checks that item against
 @racket[item-exp]. The given @racket[item-exp] is converted to an expectation
 with @racket[->expectation].
 @(expect-examples
   (define expect-second-string? (expect-vector-ref (expect-pred string?) 1))
   (expect! #(10 "text") expect-second-string?)
   (eval:error (expect! #(10 20) expect-second-string?)))}

@defproc[(expect-vector-count
          [count-exp (or/c exact-nonnegative-integer? expectation?)])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a vector,
 then checks the number of items in the vector against @racket[count-exp]. If
 @racket[count-exp] is an integer, it is converted to an expectation with
 @racket[->expectation].
 @(expect-examples
   (define expect-even-vector (expect-vector-count (expect-pred even?)))
   (expect! #(a b) expect-even-vector)
   (eval:error (expect! #(a b c) expect-even-vector)))}

@defproc[(expect-set [v any/c] ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a set that
 contains exactly the given @racket[v] values and no other values. The
 expectation finds one @fault-tech{fault} for each extra item and for each
 missing item in the checked set. @bold{This function does not convert its
  arguments to expectations}, see @racket[->expectation].
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

@defproc[(expect-set-count
          [count-exp (or/c exact-nonnegative-integer? expectation?)])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a set whose
 number of elements is then checked against @racket[count-exp]. If
 @racket[count-exp] is an integer, it is converted to an expectation with
 @racket[->expectation].
 @(expect-examples
   (expect! (set 'foo 'bar) (expect-set-count 2))
   (eval:error (expect! (set 1 2 3) (expect-set-count (expect-pred even?)))))}

@defproc[(expect-hash [k any/c] [value-exp any/c] ... ...) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a hash that
 contains exactly the given @racket[k] keys and, for each key, contains a value
 that is then checked against the corresponding @racket[value-exp]. Each
 @racket[value-exp] is converted to an expectation with @racket[->expectation].
 Extra or missing keys result in @fault-tech{faults}.
 @(expect-examples
   (expect! (hash 'a 1 'b 2) (expect-hash 'a 1 'b 2))
   (eval:error (expect! (hash 'a 1 'c 3) (expect-hash 'a 1 'b 2)))
   (eval:error (expect! (hash 'a 1 'b 1000) (expect-hash 'a 1 'b 2))))}

@defproc[(expect-hash-ref [k any/c] [value-exp any/c]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a hash that
 contains @racket[k], then checks the value for @racket[k] against
 @racket[value-exp]. The given @racket[value-exp] is converted to an expectation
 with @racket[->expectation].
 @(expect-examples
   (expect! (hash 'a 1 'b 2) (expect-hash-ref 'a 1))
   (eval:error (expect! (hash 'a 100) (expect-hash-ref 'a 1)))
   (eval:error (expect! (hash 'b 2) (expect-hash-ref 'a 1))))}

@defproc[(expect-hash-count
          [count-exp (or/c exact-nonnegative-integer? expectation?)])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a hash whose
 number of key-value pairs is then checked against @racket[count-exp]. If
 @racket[count-exp] is an integer, it is converted to an expectation with
 @racket[->expectation].
 @(expect-examples
   (expect! (hash 'a 1 'b 2) (expect-hash-count 2))
   (eval:error (expect! (hash 'a 1) (expect-hash-count (expect-pred even?)))))}

@defproc[(expect-hash-keys [set-exp (or/c set? expectation?)]) expectation?]{
 Returns an @expectation-tech{expectation} that expects a value is a hash whose
 set of keys is then checked against @racket[set-exp]. If @racket[set-exp] is a
 set, it is converted to an expectation with @racket[->expectation].
 @(expect-examples
   (expect! (hash 'a 1 'b 2) (expect-hash-keys (set 'a 'b)))
   (eval:error (expect! (hash 'a 1) (expect-hash-keys (set 'a 'b)))))}

@section{Data Structure Contexts and Attributes}

@deftogether[
 (@defstruct*[(dict-context context) ([key any/c])
              #:transparent #:omit-constructor]
   @defproc[(make-dict-context [key any/c]) dict-context?])]{
 A @context-tech{context} and its constructor that represents the dictionary
 value for @racket[key] in a dictionary, as defined by the @racket[gen:dict]
 interface. This context may be used in faults that only operate on specialized
 dictionaries, see @racket[expect-hash-ref] for an example.}

@deftogether[
 (@defstruct*[(sequence-context context) ([position exact-nonnegative-integer?])
              #:transparent #:omit-constructor]
   @defproc[(make-sequence-context [position exact-nonnegative-integer?])
            sequence-context?])]{
 A @context-tech{context} and its constructor that represents the sequence item
 at @racket[position] in a sequence, such as those returned by
 @racket[in-range]. Like @racket[dict-context], thiscontext may be used in
 faults that operate on specific kinds of sequences. See
 @racket[expect-list-ref] for an example.}

@deftogether[
 (@defstruct*[(member-attribute attribute) ([value any/c])
              #:transparent #:omit-constructor]
   @defproc[(make-member-attribute [value any/c]) member-attribute?])]{
 An @attribute-tech{attribute} and its constructor that represents a
 @racket[set] that has @racket[value] as a member. See
 @racket[expect-set-member?] for examples.}
