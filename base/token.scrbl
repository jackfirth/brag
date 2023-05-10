#lang scribble/manual

@(require (for-label racket/base
                     racket/contract
                     yaragg/base/token))

@title{Tokens}

@defmodule[yaragg/base/token]

A @deftech{token} is a single unit of text. Tokenizing is the act of converting a sequence of
characters into a sequence of tokens, where each character becomes part of exactly one token. Tokens
are the inputs to parsers: a parser's job is to convert a sequence of tokens into a structured parse
tree. There are four types of tokens in Yaragg: @tech{atoms}, @tech{punctuation}, @tech{whitespace},
and @tech{comments}.

@defproc[(token? [v any/c]) boolean?]

@section{Atom Tokens}

An @deftech{atom} is a @tech{token} that represents a single independent piece of text that's relevant
to parsing, such as a symbolic name or a literal number. Atoms become the leaves of a parse tree when
parsing tokens. Atoms can have types, for parsers to distinguish different kinds of atoms, and values,
which don't affect parsing but do end up embedded in the final parse tree.

@defproc[(atom? [v any/c]) boolean?]
@defproc[(atom [type symbol?] [value any/c] [#:properties properties hash? (hash)]) atom?]
@defproc[(atom-type [atom atom?]) symbol?]
@defproc[(atom-value [atom atom?]) any/c]
@defproc[(atom-properties [atom atom?]) (and/c hash? immutable?)]

@section{Punctuation Tokens}

A @deftech{punctuation} token is a @tech{token} that represents a piece of text whose purpose is to
separate or delimit other pieces of text, such as parentheses, brackets, and semicolons in various
programming languages. Parsers use punctuation when deciding how to structure @tech{atoms} into parse
trees.

@defproc[(punctuation? [v any/c]) boolean?]
@defproc[(punctuation [string string?]) punctuation?]
@defproc[(punctuation-string [punctuation punctuation?]) (and/c string? immutable?)]

@section{Whitespace Tokens}

A @deftech{whitespace} token is a @tech{token} made up of invisible whitespace characters. Whitespace
tokens only exist to separate other tokens and do not affect parsing. Adding and removing whitespace
tokens from a token stream does not change the behavior of any Yaragg parser. However, other tools
such as formatters and pretty printers may be affected by whitespace tokens.

A whitespace token always represents some number of linebreaks followed by some number of spaces. This
representation disallows trailing whitespace, which is usually desirable when creating and parsing
programming languages.

@defproc[(whitespace? [v any/c]) boolean?]

@defproc[(whitespace
          [#:linebreak-count linebreak-count exact-nonnegative-integer? 0]
          [#:space-count space-count exact-nonnegative-integer? 0])
         whitespace?]

@defproc[(whitespace-linebreak-count [space whitespace?]) exact-nonnegative-integer?]
@defproc[(whitespace-space-count [space whitespace?]) exact-nonnegative-integer?]

@section{Comment Tokens}

A @deftech{comment} token is a @tech{token} made up of arbitrary text that is only relevant to human
readers. Comments are ignored by parsers. Formatters may move comments around, but they won't remove
them.

@defproc[(comment? [v any/c]) boolean?]
@defproc[(comment [text string?]) comment?]
@defproc[(comment-text [comment comment?]) (and/c string? immutable?)]

@section{Lexemes}

A @deftech{lexeme} is a combination of a @tech{token} and information about the original source
location of that token. Parsers normally produce parse trees from token streams, but a lexeme stream
is needed when parsing input into syntax objects with source locations.

@defproc[(lexeme? [v any/c]) boolean?]
@defproc[(lexeme [token token?] [location srcloc?]) lexeme?]
@defproc[(lexeme-token [lexeme lexeme?]) token?]
@defproc[(lexeme-location [lexeme lexeme?]) srcloc?]
