#lang scribble/manual

@(require "base.rkt")

@title{Logger Expectations}

@defproc[(expect-log* [exp any/c] [#:logger logger logger? (current-logger)])
         expectation?]{
 Returns an @expectation-tech{expectation} that expects a thunk. That thunk is
 called with a new @log-receiver-tech{log receiver} listening to
 @racket[logger], and the list of log events (described in
 @racket[make-log-receiver]) it receives is checked against @racket[exp]. If
 @racket[exp] is not an expectation, it is converted to one with
 @racket[->expectation]. Note that due to how loggers and log receivers work, it
 is not possible to distinguish messages sent to @racket[logger] by the input
 thunk from messages sent to @racket[logger] in any other threads that happen to
 be running while the input thunk is being called. This makes it unsafe to run
 multiple tests that test the same logger concurrently. However, those tests can
 be run in parallel in different places because logger state is local to each
 place.

 @(expect-examples
   (struct foo-data (value) #:transparent)
   (define (log-foo)
     (log-message (current-logger)
                  'info
                  'foo-topic
                  "log message from foo"
                  (foo-data 'some-data)))
   (define foo-msg
     (vector 'info
             "foo-topic: log message from foo"
             (foo-data 'some-data)
             'foo-topic))
   (expect! log-foo (expect-log* (list foo-msg)))
   (eval:error (expect! void (expect-log (list foo-msg)))))}

@defthing[the-log-context context?]{
 A @context-tech{context} that represents the list of messages sent to a logger
 during the evaluation of a thunk.}
