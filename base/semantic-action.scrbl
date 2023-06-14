#lang scribble/manual


@(require (for-label racket/base
                     racket/contract
                     yaragg/base/semantic-action))


@title{Semantic Actions}
@defmodule[yaragg/base/semantic-action]


A @deftech{semantic action} is a type of tree transformation that describes how to transform a node in
a @tech{derivation tree} into a node in a @tech{syntax object} or S-expression. Semantic actions can
join child trees into lists, insert labels, add metadata in the form of syntax properties, splice
subtrees together, or even delete entire branches of the derivation tree.


@defproc[(semantic-action? [v any/c]) boolean?]{
 A predicate for @tech{semantic actions}.}


@defthing[splice-action semantic-action?]{
 A @tech{semantic action} that splices a @tech{derivation}'s subtrees together. Splice actions are
 useful for omitting internal parse tree nodes when they represent structure that's only needed during
 parsing.}


@defthing[cut-action semantic-action?]{
 A @tech{semantic action} omits a @tech{derivation} entirely. Cut actions are useful for omitting
 subtrees that don't represent useful information and only exist to facilitate parsing.}


@defproc[(label-action
          [label any/c]
          [#:label-properties label-properties hash? (hash)]
          [#:expression-properties expression-properties hash? (hash)])
         semantic-action?]{
 A @tech{semantic action} that builds a list out of a @tech{derivation}'s children, with
 @racket[label] inserted into the head of the list. Label actions are useful for inserting artificial
 identifiers that weren't part of the original parsed input.}
