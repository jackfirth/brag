#lang scribble/manual


@(require (for-label racket/base
                     racket/contract
                     yaragg/base/derivation
                     yaragg/base/token
                     yaragg/base/semantic-action))


@title{Derivation Trees}
@defmodule[yaragg/base/derivation]


A @deftech{derivation tree}, or simply a @deftech{derivation}, is a tree that represents the structure
a @tech{parser} derived when parsing an input string according to some @tech{grammar}. Derivation
trees are similar to abstract syntax trees, but they tend to include more details that matter only to
parsing. Derivation trees can be transformed into either @tech{syntax objects} or plain S-expressions.
Nodes within derivation trees can be labeled with @tech{semantic actions} in order to transform the
syntax objects and S-expressions produced from the tree.


@defproc[(parser-derivation? [v any/c]) boolean?]{
 A predicate for @tech{derivation}s.}


@defproc*[([(parser-derivation [token token?]) parser-derivation?]
           [(parser-derivation [action semantic-action?] [child parser-derivation?] ...+)
            parser-derivation?])]{
 Constructs a @tech{derivation}. In the first case, a terminal derivation matching a single input
 token is constructed. In the second case, a nonterminal derivation containing each of the
 @racket[child] derivations is constructed. The given @racket[action] is applied to the children when
 the derivation is converted to a syntax object or S-expression.}


@defproc[(parser-derivation->syntax [derivation parser-derivation?]) syntax?]{
 Converts @racket[derivation] into a @tech{syntax object}.}


@defproc[(parser-derivation->datum [derivation parser-derivation?]) any/c]{
 Converts @racket[derivation] into an S-expression.}
