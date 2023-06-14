#lang scribble/manual


@(require (for-label racket/base
                     racket/contract
                     racket/sequence
                     yaragg/parser))


@title{Parsers}
@defmodule[yaragg/parser]


A @deftech{parser} parses sequences of @tech{tokens} into parse trees represented by
@tech{derivation trees}, @tech{syntax objects}, or S-expressions. In Yaragg, each parser is specific
to a particular @tech{grammar}. Parser construction functions like @racket[earley-parser] consume the
grammar as an input and produce a parser for that grammar.


@defproc[(parser? [v any/c]) boolean?]{
 A predicate for @tech{parsers}.}


@defproc[(parse-syntax [parser parser?] [tokens (sequence/c token?)]) syntax?]{
 Parses @racket[tokens] into a @tech{syntax object} using @racket[parser].}


@defproc[(parse-datum [parser parser?] [tokens (sequence/c token?)]) any/c]{
 Parses @racket[tokens] into an S-expression using @racket[parser].}


@section{Earley parsing}
@defmodule[yaragg/parser/earley]


@racket[(earley-parser [grammar grammar?]) parser?]{
 Constructs a parser for @racket[grammar] implemented using the Earley parsing algorithm.}
