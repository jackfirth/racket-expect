#lang scribble/manual

@(require "base.rkt")

@title{Data Model}

Conceptually, an @expectation-tech[#:definition? #t]{expectation} is a function
that returns a list of @fault-tech[#:definition? #t]{faults} about its input.
Faults are a difference between some expected @attribute-tech{attribute} of the
input and an actual attribute. Faults are often scoped to a
@context-tech{context}, which identifies where precisely in the input value the
discrepency exists. This section documents each of these structures and how to
use their basic functionalities.

@defproc[(expectation? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is an @expectation-tech{expectation}, returns
 @racket[#f] otherwise.
 @(expect-examples
   (expectation? 6)
   (expectation? expect-true))}

@defproc[(expectation [proc (-> any/c (listof fault?))]) expectation?]{
 Returns an @expectation-tech{expectation} whose implementation is
 @racket[proc].
 @(expect-examples
   (define empty-expectation (expectation (λ (v) (list))))
   (expectation-apply empty-expectation 'foo))}

@defproc[(expectation-apply [exp expectation?] [v any/c]) (listof faults?)]{
 Checks @racket[v] against @racket[exp] and returns a list of faults found by
 @racket[exp].
 @(expect-examples
   (expectation-apply (expect-equal? (list 1 2)) (list 1 2))
   (expectation-apply (expect-equal? (list 1 2)) (list 1 'foo)))}

@defproc[(expect! [v any/c] [exp any/c]) void?]{
 Checks that @racket[v] has no @fault-tech{faults} according to @racket[exp]. If
 it does, an instance of @racket[exn:fail:expect] is raised with a message
 detailing the faults. If @racket[exp] is not an expectation, it is converted to
 one with @racket[->expectation].
 @(expect-examples
   (expect! '(1 2) '(1 2))
   (eval:error (expect! '(1 a b) '(1 2))))}

@defstruct*[(exn:fail:expect exn:fail)
            ([subject any/c] [faults (listof fault?)])]{
 An instance of @racket[exn:fail] that is thrown by @racket[expect!] when a
 value does not live up to an @expectation-tech{expectation}. The
 @racket[subject] field is the original value checked against the expectation
 and the @racket[faults] field is the list of @fault-tech{faults} found by the
 expectation.}

@defthing[expect-any expectation?]{
 The empty expectation. Finds no @fault-tech{faults} in any value. Not very
 useful on it's own, but this is sometimes useful in higher order contexts (such
 as a default argument).}

@defproc[(fault? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is a @fault-tech{fault}, returns @racket[#f]
 otherwise.
 @(expect-examples
   (fault? 6)
   (fault? (fault #:summary "test fault"
                  #:expected (self-attribute 'foo)
                  #:actual (self-attribute 'bar))))}

@defproc[(fault [#:summary summary string?]
                [#:expected expected attribute?]
                [#:actual actual attribute?]
                [#:contexts contexts (listof context?) (list)])
         fault?]{
 Returns a @fault-tech{fault} with the given summary, expected and actual
 attributes, and contexts. Contexts are given in order of least specific to most
 specific; see @racket[expect-list] for an example of proper use of contexts.
 @(expect-examples
   (fault #:summary "test fault"
          #:expected (self-attribute 'foo)
          #:actual (self-attribute 'bar))
   (struct test-context context () #:transparent)
   (fault #:summary "test fault with contexts"
          #:expected (self-attribute 'foo)
          #:actual (self-attribute 'bar)
          #:contexts (list (test-context "test context")
                           (test-context "nested test context"))))}

@deftogether[
 (@defproc[(fault-summary [flt fault?]) string?]
   @defproc[(fault-expected [flt fault?]) attribute?]
   @defproc[(fault-actual [flt fault?]) attribute?]
   @defproc[(fault-contexts [flt fault?]) (listof context?)])]{
 Accessors for the various fields of a @racket[fault?] structure. See
 @racket[fault] for information about these fields.}

@defstruct*[context ([description string?]) #:transparent #:omit-constructor]{
 A structure type for what @context-tech[#:definition? #t]{context} a
 @fault-tech{fault} occurs in. Contexts are meant to be structured information,
 so that different clients can render descriptions of contexts in different
 ways. Every context includes a @racket[description] string so that a simple
 text description of the context can be constructed. The @racket[context]
 constructor is not provided; it's not possible to create a context without
 defining a subtype of the @racket[context] struct. Various expectations
 provided by this library define and provide their own context subtypes that add
 extra fields, see @racket[expect-list] for an example.}

@defstruct*[attribute ([description string?]) #:transparent #:omit-constructor]{
 A structure type for an @attribute-tech[#:definition? #t]{attribute}, some
 property of a value that a @fault-tech{fault} refers to. Like
 @context-tech{contexts}, attributes are structured information with a string
 description whose subtypes add additional fields for clients to use to
 construct error messages. Additionally, the @racket[attribute] constructor is
 not provided so the only way to create attributes is with a subtype. See
 @racket[self-attribute] for a trivial implementation.}

@defproc[(self-attribute? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is an @attribute-tech{attribute} returned by
 @racket[self-attribute], returns @racket[#f] otherwise.
 @(expect-examples
   (self-attribute? 'nope)
   (self-attribute? (self-attribute 'yup)))}

@defproc[(self-attribute [value any/c]) self-attribute?]{
 Constructs an @attribute-tech{attribute} whose description is
 @racket[(~v value)]. The @racket[value] used to construct the attribute can be
 retrieved later with @racket[self-attribute-value].
 @(expect-examples
   (self-attribute 'foo)
   (attribute-description (self-attribute 'foo)))}

@defproc[(self-attribute-value [self-attr self-attribute?]) any/c]{
 Returns the original value used to construct @racket[self-attr] with
 @racket[self-attribute].
 @(expect-examples
   (self-attribute-value (self-attribute 'foo)))}
