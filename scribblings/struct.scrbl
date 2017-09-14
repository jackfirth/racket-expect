#lang scribble/manual

@(require "base.rkt")

@title{Structure Expectations}

@defform[(expect-struct id [accessor-id expect-expr] ...)
         #:contracts ([expect-expr any/c])]{
 Creates an @expectation-tech{expectation} that checks a value is an instance of
 the structure type @racket[id], then checks the value of applying each
 @racket[accessor-id] to the struct with the corresponding @racket[expect-expr].
 If any @racket[expect-expr] is not an expectation, it is converted to one with
 @racket[->expectation]. Not all accessors of @racket[id] need to be provided;
 extra fields in structures checked by the expectation do not cause any
 @fault-tech{faults}. Accessors may be provided in any order.

 The @racket[id] must have a transformer binding to a @racket[struct-info?]
 value, and that value must supply the structure type's predicate. Accessors of
 the struct's supertypes are allowed. Faults found by the expectation in
 accessed fields have a @racket[struct-accessor-context] value added to their
 @context-tech{contexts}.

 @(expect-examples
   (struct fish (color weight) #:transparent)
   (eval:error
    (expect! (fish 'red 5) (expect-struct fish [fish-color 'blue]))))}

@defform[(define-struct-expectation struct-maybe-id)
         #:grammar ([struct-maybe-id struct-id (id struct-id)])]{
 Binds @racket[id] to a procedure that constructs
 @expectation-tech{expectations} with @racket[expect-struct]. The bound
 procedure accepts one keyword argument for each non-inherited field of
 @racket[struct-id] and passes it to @racket[struct-id]. If @racket[id] is not
 provided, it defaults to @racket[expect-]@racket[struct-id]. All keyword
 arguments to the bound procedure are optional; if not provided they default to
 @racket[expect-any].

 Like @racket[expect-struct], @racket[struct-id] must have a transformer binding
 to a @racket[struct-info?], which is inspected by 
 @racket[define-struct-expectation] to determine what accessors to generate
 keyword arguments for. All accessors must be of the pattern
 @racket[struct-id]-@racket[field-id] (such as fish-color) or an "ambiguous
 keyword form" syntax error is reported. For accessors matching this pattern,
 the corresponding keyword used by the bound procedure is the symbol
 @racket['field-id] converted to a keyword.

 @(expect-examples
   (struct fish (color weight) #:transparent)
   (define-struct-expectation fish)
   (eval:error (expect! (fish 'red 5) (expect-fish #:weight 20))))}

@defstruct*[(struct-accessor-context context) ([accessor-id identifier?])
            #:transparent #:omit-constructor]{
 A @context-tech{context} that indicates a fault lies in a struct field defined
 by @racket[accessor-id].}

@defproc[(make-struct-accessor-context [accessor-id identifier?])
         struct-field-context?]{
 Returns a @racket[struct-accessor-context] with a default
 @racket[context-description] string referencing @racket[accessor-id].

 @(expect-examples
   (make-struct-accessor-context #'shape-area))}
