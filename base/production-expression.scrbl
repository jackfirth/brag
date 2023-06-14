#lang scribble/manual


@(require (for-label racket/base
                     racket/contract
                     racket/sequence
                     yaragg/base/production-expression))


@title{Production expressions}
@defmodule[yaragg/base/production-expression]


A @deftech{production expression} is a pattern of symbols and tokens in a @tech{grammar}. Production
expressions are used by @tech{production rules} to describe what pattern of tokens a symbol
corresponds to. Production expressions come in a few variants:


@itemlist[

 @item{A @deftech{group expression} matches a group of subexpressions in series.}

 @item{A @deftech{choice expression} tries subexpressions one at a time and selects the first matching
  subexpression.}

 @item{A @deftech{repetition expression} matches a single subexpression multiple times in series.}

 @item{A @deftech{cut expression} matches a subexpression but omits the match from the parse tree.}

 @item{A @deftech{terminal symbol}, which matches a single token in the input stream.}

 @item{A @deftech{nonterminal symbol}, which refers to other production expressions as defined by the
  production rules of the grammar.}]


@defproc[(production-expression? [v any/c]) boolean?]{
 A predicate for @tech{production expressions}.}

@defproc[(group-expression? [v any/c]) boolean?]{
 A predicate for @tech{group expressions}.}

@defproc[(group-expression [subexpressions (sequence/c production-expression?)]) group-expression?]{
 Constructs a @tech{group expression}, which is a @tech{production expression} that matches each
 expression in @racket[subexpressions] one after the other in series. Every subexpression must match
 for the entire group expression to match.}

@defproc[(choice-expression? [v any/c]) boolean?]{
 A predicate for @tech{choice expressions}.}

@defproc[(choice-expression [choices (sequence/c (or/c production-expression? grammar-symbol?))])
         choice-expression?]{
 Constructs a @tech{choice expression}, which is a @tech{production expression} that attempts each
 expression in @racket[choices] on the input until one of them matches. At least one subexpression
 must match for the entire choice expression to match.}

@defproc[(repetition-expression? [v any/c]) boolean?]{
 A predicate for @tech{repetition expressions}.}

@defproc[(repetition-expression
          [subexpression (or/c production-expression? grammar-symbol?)]
          [#:min-count min-count exact-nonnegative-integer? 0]
          [#:max-count max-count (or/c exact-nonnegative-integer? +inf.0) +inf.0])
         repetition-expression?]{
 Constructs a @tech{repetition expression}, which is a @tech{production expression} that attempts to
 match @racket[subexpression] multiple times in series. The subexpression must match at least
 @racket[min-count] times and at most @racket[max-count] times.}

@defproc[(cut-expression? [v any/c]) boolean?]{
 A predicate for @tech{cut expressions}.}

@defproc[(cut-expression [subexpression (or/c production-expression? grammar-symbol?)])
         cut-expression?]{
 Constructs a @tech{cut expression}, which is a @tech{production expression} that functions exactly
 like @racket[subexpression] except the match is omitted from the resulting parse tree.}

@defproc[(grammar-symbol? [v any/c]) boolean?]{
 A predicate for @deftech{grammar symbols}, which are either @tech{terminal symbols} or
 @tech{nonterminal symbols}.}

@defproc[(atom-symbol? [v any/c]) boolean?]

@defproc[(atom-symbol [type symbol?]) atom-symbol?]

@defproc[(punctuation-symbol? [v any/c]) boolean?]

@defproc[(punctuation-symbol [str string?]) punctuation-symbol?]

@defproc[(nonterminal-symbol? [v any/c]) boolean?]

@defproc[(nonterminal-symbol [key any/c]) nonterminal-symbol?]
