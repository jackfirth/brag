#lang scribble/manual


@(require (for-label yaragg))


@title{Yaragg: Yet Another Racket AST-Generator Generator}


This package is a library for building Racket languages with non-S-expression syntax. Currently it
provides a parser generator that consumes a grammar and produces Racket S-expressions and syntax
objects. This package is a wildly incomplete hobby project inspired by
@racketmodname[ragg #:indirect] and @racketmodname[brag #:indirect]. Those packages are likely what
you want if you're looking for something more fleshed out and properly supported.


@local-table-of-contents[]


@include-section[(lib "yaragg/base/token.scrbl")]
@include-section[(lib "yaragg/base/grammar.scrbl")]
@include-section[(lib "yaragg/base/production-expression.scrbl")]
@include-section[(lib "yaragg/base/derivation.scrbl")]
@include-section[(lib "yaragg/base/semantic-action.scrbl")]
