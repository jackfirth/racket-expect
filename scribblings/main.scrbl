#lang scribble/manual

@(require "base.rkt")

@title{Expect: First Class Assertions}
@defmodule[expect #:packages ("expect")]
@author[@author+email["Jack Firth" "jackhfirth@gmail.com"]]

This library provides @expectation-tech{expectations},
composable first-class representations of assertions that should be true of some
value. This library integrates with RackUnit to provide checks in terms of
expectations, making it simpler to create custom checks with high-quality error
messages.

@source-code-link{https://github.com/jackfirth/racket-expect}

@local-table-of-contents[]

@include-section["base.scrbl"]
@include-section["data.scrbl"]
