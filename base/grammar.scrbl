#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     yaragg/base/grammar
                     yaragg/base/production-expression
                     yaragg/base/semantic-action))

@title{Grammars}
@defmodule[yaragg/base/grammar]


A @deftech{grammar} is a formal description of the syntax of a language. Grammars consist of a start
symbol and a set of @tech{production rules} which describe how to rewrite the start symbol into valid
strings in the language. Grammars are used to construct parsers, which can transform input strings
into a parse tree structured according to the rules laid out in the grammar.


@defproc[(grammar? [v any/c]) boolean?]{
 A predicate for @tech{grammars}.}

@defproc[(grammar
          [#:rules rules (sequence/c production-rule?)]
          [#:start-symbol start-symbol nonterminal-symbol?])
         grammar?]{
 Constructs a @tech{grammar} containing @racket[rules] and using @racket[start-symbol] to identify the
 initial rules to apply during parsing.}


@section{Production rules}


A @deftech{production rule} is a rule in a @tech{grammar} that describes how a nonterminal symbol in
the grammar can be rewritten into a string of other grammar symbols and input @tech{tokens}. The
rewrite pattern is represented by a @tech{production expression}.


@defproc[(production-rule? [v any/c]) boolean?]{
 A predicate for @tech{production rules}.}

@defproc[(production-rule
          [#:nonterminal nonterminal nonterminal-symbol?]
          [#:substitution substitution production-expression?]
          [#:action action semantic-action?])
         production-rule?]{
 Constructs a @tech{production rule} that rewrites @racket[nonterminal] into @racket[substitution].
 During parsing, @racket[action] is applied to parse nodes created by the constructed rule.}
