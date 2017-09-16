#lang scribble/manual

@(require "base.rkt")

@title{Macro Expansion Expectations}

@defproc[(expect-expand [exp expectation?]
                        [#:namespace ns namespace? (current-namespace)])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a syntax object object
 @racket[stx], then a thunk that evaluates @racket[(expand stx)] is created and
 checked against @racket[exp]. The call to @racket[expand] is made with
 @racket[current-namespace] parameterized to @racket[ns]. Combine this with
 @racket[expect-raise] to test that a specific syntax error is made, or  combine
 with @racket[expect-return] to test properties of the resulting fully expanded
 syntax. See also @racket[expect-syntax-exn].

 @(expect-examples
   (define success #f)
   (define-syntax-rule (foo (id v) ...) success)
   (expect! #'(foo (a 1) (b 2))
            (expect-expand (expect-return (expect-syntax 'success))))
   (eval:error (expect! #'(foo a) (expect-expand expect-not-raise))))}

@defproc[(expect-expand-once [exp expectation?]
                             [#:namespace ns namespace? (current-namespace)])
         expectation?]{
 Like @racket[expect-expand], but calls @racket[expand-once] on the input syntax
 object instead of @racket[expand].}

@defproc[(expect-syntax-exn
          [msg-exp (or/c string? regexp? expectation?) expect-any]
          [#:namespace ns namespace? (current-namespace)])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a syntax object and
 expects that expanding that syntax object raises an @racket[exn:fail:syntax]
 value whose message is checked against @racket[msg-exp]. The syntax object is
 expanded with @racket[current-namespace] parameterized to @racket[ns]. If
 @racket[msg-exp] is a regexp, it is converted to an expectation with
 @racket[expect-regexp-match]. Otherwise, it is converted with
 @racket[->expectation]. This procedure is essentially sugar over combining
 @racket[expect-expand], @racket[expect-raise], @racket[expect-struct], and
 @racket[expect-regexp-match] manually.

 @(expect-examples
   (expect! #'(let ([a 1] [a 2]) (void)) (expect-syntax-exn #rx"duplicate")))}
