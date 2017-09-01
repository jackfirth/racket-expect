#lang scribble/manual

@(require "base.rkt")

@title{Structure Expectations}

@defform[(expect-struct id [field-id expect-expr] ...)
         #:contracts ([expect-expr any/c])]{
 Creates an @expectation-tech{expectation} that checks a value is an instance of
 the structure type @racket[id], then checks the value of each @racket[field-id]
 of the struct with the corresponding @racket[expect-expr]. If any
 @racket[expect-expr] is not an expectation, it is converted to one with
 @racket[->expectation]. Not all fields of @racket[id] need to be provided;
 extra fields in structures checked by the expectation do not cause any
 @fault-tech{faults}. Fields may be provided in any order.

 The @racket[id] must have a transformer binding to a @racket[struct-info?]
 value, and that value must supply the structure type's predicate and all field
 accessors. Faults found by the expectation in fields have a
 @racket[struct-field-context] value added to their @context-tech{contexts}.

 @(expect-examples
   (struct fish (color weight) #:transparent)
   (eval:error (expect! (fish 'red 5) (expect-struct fish [color 'blue]))))}

@defform[(define-struct-expectation struct-maybe-id (field ...))
         #:grammar ([struct-maybe-id struct-id (id struct-id)]
                    [field field-id [field-kw field-id]])]{
 Binds @racket[id] to a procedure that accepts the @racket[field-kw] keyword
 arguments and returns an expectation with @racket[expect-struct],
 @racket[struct-id], and each @racket[field-kw] argument paired with its
 corresponding @racket[field-id]. If @racket[id] is not provided, it defaults to
 @racket[expect-]@racket[struct-id]. All keyword arguments to @racket[id] are
 optional; if not provided they default to @racket[expect-any]. If a
 @racket[field-kw] is not provided, it defaults to the corresponding
 @racket[field-id] converted to a keyword.

 Like @racket[expect-struct], @racket[struct-id] must have a transformer binding
 to a @racket[struct-info?] value and each @racket[field-id] must be bound to a
 field accessor when combined with @racket[struct-id].

 @(expect-examples
   (struct fish (color weight) #:transparent)
   (define-struct-expectation fish (color weight))
   (eval:error (expect! (fish 'red 5) (expect-fish #:weight 20))))}

@defstruct*[(struct-field-context context)
            ([type-id identifier?] [field-id identifier?])
            #:transparent
            #:omit-constructor]{
 A @context-tech{context} that indicates a fault lies in the value of a
 @racket[field-id] field of a structure with type @racket[type-id].}

@defproc[(make-struct-field-context [type-id identifier?]
                                    [field-id identifier?])
         struct-field-context?]{
 Returns a @racket[struct-field-context] with a default
 @racket[context-description] string referencing @racket[type-id] and
 @racket[field-id].

 @(expect-examples
   (make-struct-field-context #'shape #'area))}
