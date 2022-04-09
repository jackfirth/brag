#lang racket/base

;; Provides the syntax used to create lexers and the functions needed to
;; create and use the buffer that the lexer reads from.  See docs.

(require (for-syntax racket/base
                     racket/contract/base
                     syntax/define
                     racket/match
                     racket/promise
                     syntax/stx
                     racket/syntax
                     yaragg/parser-tools/private-lex/actions
                     yaragg/parser-tools/private-lex/front
                     yaragg/parser-tools/private-lex/unicode-chars
                     yaragg/parser-tools/private-lex/util)
         racket/contract/base
         racket/stxparam
         syntax/parse/define
         syntax/readerr
         yaragg/parser-tools/private-lex/token)


(provide lexer
         lexer-src-pos
         lexer-srcloc
         define-lex-abbrev
         define-lex-abbrevs
         define-lex-trans
         define-tokens
         define-empty-tokens
         token-name
         token-value
         token?
         (struct-out position)
         (struct-out position-token)
         (struct-out srcloc-token)
         file-path
         lexer-file-path
         any-char
         any-string
         nothing
         alphabetic
         lower-case
         upper-case
         title-case
         numeric
         symbolic
         punctuation
         graphic
         whitespace
         blank
         iso-control
         char-set
         start-pos
         end-pos
         lexeme
         lexeme-srcloc
         input-port
         return-without-pos
         return-without-srcloc)

;; wrap-action: syntax-object src-pos? -> syntax-object
(define-for-syntax (wrap-action action src-loc-style)
  (define/with-syntax action-stx
    (cond
      [(eq? src-loc-style 'lexer-src-pos)
       #`(let/ec ret
           (syntax-parameterize ([return-without-pos (make-rename-transformer #'ret)])
             (position-token #,action start-pos end-pos)))]
      [(eq? src-loc-style 'lexer-srcloc)
       #`(let/ec ret
           (syntax-parameterize ([return-without-srcloc (make-rename-transformer #'ret)])
             (srcloc-token #,action lexeme-srcloc)))]
      [else action]))
  (syntax/loc action
    (λ (start-pos-p end-pos-p lexeme-p input-port-p)
      (define lexeme-srcloc-p (make-srcloc (object-name input-port-p)
                                           (position-line start-pos-p)
                                           (position-col start-pos-p)
                                           (position-offset start-pos-p)
                                           (and (number? (position-offset end-pos-p))
                                                (number? (position-offset start-pos-p))
                                                (- (position-offset end-pos-p)
                                                   (position-offset start-pos-p)))))
      (syntax-parameterize 
          ([start-pos (make-rename-transformer #'start-pos-p)]
           [end-pos (make-rename-transformer #'end-pos-p)]
           [lexeme (make-rename-transformer #'lexeme-p)]
           [input-port (make-rename-transformer #'input-port-p)]
           [lexeme-srcloc (make-rename-transformer #'lexeme-srcloc-p)])
        action-stx))))

(define-for-syntax (make-lexer-macro caller src-loc-style)
  (λ (stx)
    (syntax-case stx ()
      [(_ . RE+ACTS)         
       (with-disappeared-uses
           (define spec/re-acts (syntax->list #'RE+ACTS))
         (for/and ([x (in-list spec/re-acts)])
           (syntax-case x ()
             [(RE ACT) #t]
             [else (raise-syntax-error caller "not a regular expression / action pair" stx x)]))
         (define eof-act
           (get-special-action spec/re-acts
                               #'eof
                               (case src-loc-style
                                 [(lexer-src-pos) #'(return-without-pos eof)]
                                 [(lexer-srcloc) #'(return-without-srcloc eof)]
                                 [else #'eof])))
         (define spec-act (get-special-action spec/re-acts #'special #'(void)))
         (define spec-comment-act (get-special-action spec/re-acts #'special-comment #'#f))
         (define ids (list #'special #'special-comment #'eof))
         (define re-acts
           (filter (λ (spec/re-act)
                     (syntax-case spec/re-act ()
                       [((special) act)
                        (not (for/or ([x (in-list ids)])
                               (and (identifier? #'special)
                                    (module-or-top-identifier=? #'special x))))]
                       [_ #t]))
                   spec/re-acts))
         (define names (map (λ (x) (datum->syntax #f (gensym))) re-acts))
         (define acts (map (λ (x) (stx-car (stx-cdr x))) re-acts))
         (define re-actnames (map (λ (re-act name) (list (stx-car re-act) name)) re-acts names))
         (when (null? spec/re-acts)
           (raise-syntax-error caller "expected at least one action" stx))
         (define-values (trans start action-names no-look) (build-lexer re-actnames))
         (when (vector-ref action-names start) ;; Start state is final
           (unless (and 
                    ;; All the successor states are final
                    (vector? (vector-ref trans start))
                    (andmap (λ (x) (vector-ref action-names (vector-ref x 2)))
                            (vector->list (vector-ref trans start)))
                    ;; Each character has a successor state
                    (let loop ([check 0]
                               [nexts (vector->list (vector-ref trans start))])
                      (cond
                        [(null? nexts) #f]
                        [else
                         (let ([next (car nexts)])
                           (and (= (vector-ref next 0) check)
                                (let ([next-check (vector-ref next 1)])
                                  (or (>= next-check max-char-num)
                                      (loop (add1 next-check) (cdr nexts))))))])))
             (eprintf "warning: lexer at ~a can accept the empty string\n" stx)))
         (with-syntax ([START-STATE-STX start]
                       [TRANS-TABLE-STX trans]
                       [NO-LOOKAHEAD-STX no-look]
                       [(NAME ...) names]
                       [(ACT ...) (map (λ (a) (wrap-action a src-loc-style)) acts)]
                       [(ACT-NAME ...) (vector->list action-names)]
                       [SPEC-ACT-STX (wrap-action spec-act src-loc-style)]
                       [HAS-COMMENT-ACT?-STX (and (syntax-e spec-comment-act) #t)]
                       [SPEC-COMMENT-ACT-STX (wrap-action spec-comment-act src-loc-style)]
                       [EOF-ACT-STX (wrap-action eof-act src-loc-style)])
           (syntax/loc stx (let ([NAME ACT] ...)
                             (let ([proc (lexer-body START-STATE-STX 
                                                     TRANS-TABLE-STX
                                                     (vector ACT-NAME ...)
                                                     NO-LOOKAHEAD-STX
                                                     SPEC-ACT-STX
                                                     HAS-COMMENT-ACT?-STX
                                                     SPEC-COMMENT-ACT-STX
                                                     EOF-ACT-STX)])
                               ;; reverse eta to get named procedures:
                               (λ (port) (proc port)))))))])))

(define-syntax lexer (make-lexer-macro 'lexer #f))
(define-syntax lexer-src-pos (make-lexer-macro 'lexer-src-pos 'lexer-src-pos))
(define-syntax lexer-srcloc (make-lexer-macro 'lexer-srcloc 'lexer-srcloc))
    
(define-syntax-parse-rule (define-lex-abbrev NAME:id RE)
  (define-syntax NAME
    (lex-abbrev (λ () (quote-syntax RE)))))

(define-syntax-parse-rule (define-lex-abbrevs (id:id re) ...)
  (begin (define-lex-abbrev id re) ...))

(define-syntax (define-lex-trans stx)
  (syntax-parse stx
    [(_ id:id trans-expr)
     #'(define-syntax id (lex-trans trans-expr))]
    [(_ (id:id stx-id:id) body-expr:expr)
     #'(define-syntax id (lex-trans (λ (stx-id) body-expr)))]))
       

(define (get-next-state-helper char min max table)
  (cond
    [(>= min max) #f]
    [else
     (define try (quotient (+ min max) 2))
     (define el (vector-ref table try))
     (define r1 (vector-ref el 0))
     (define r2 (vector-ref el 1))
     (cond
       [(<= r1 char r2) (vector-ref el 2)]
       [(< char r1) (get-next-state-helper char min try table)]
       [else (get-next-state-helper char (add1 try) max table)])]))




(define (get-next-state char table)
  (and table (get-next-state-helper char 0 (vector-length table) table)))
  
(define ((lexer-body start-state trans-table actions no-lookahead special-action
                     has-special-comment-action? special-comment-action eof-action) ip)
  (define (lexer ip)
    (define first-pos (get-position ip))
    (define first-char (peek-char-or-special ip 0))
    (cond
      [(eof-object? first-char)
       (do-match ip first-pos eof-action (read-char-or-special ip))]
      [(special-comment? first-char)
       (read-char-or-special ip)
       (cond
         (has-special-comment-action?
          (do-match ip first-pos special-comment-action #f))
         (else (lexer ip)))]
      [(not (char? first-char))
       (do-match ip first-pos special-action (read-char-or-special ip))]
      [else
       (let lexer-loop (
                        ;; current-state
                        [state start-state]
                        ;; the character to transition on
                        [char first-char]
                        ;; action for the longest match seen thus far
                        ;; including a match at the current state
                        [longest-match-action 
                         (vector-ref actions start-state)]
                        ;; how many bytes precede char
                        [length-bytes 0]
                        ;; how many characters have been read
                        ;; including the one just read
                        [length-chars 1]
                        ;; how many characters are in the longest match
                        [longest-match-length 0])
         (define next-state 
           (cond
             [(not (char? char)) #f]
             [else (get-next-state (char->integer char)
                                   (vector-ref trans-table state))]))
         (cond
           [(not next-state)
            (check-match ip first-pos longest-match-length
                         length-chars longest-match-action)]
           [(vector-ref no-lookahead next-state)
            (define act (vector-ref actions next-state))
            (check-match ip 
                         first-pos 
                         (if act length-chars longest-match-length)
                         length-chars
                         (if act act longest-match-action))]
           [else
            (define act (vector-ref actions next-state))
            (define next-length-bytes (+ (char-utf-8-length char) length-bytes))
            (define next-char (peek-char-or-special ip next-length-bytes))
            (lexer-loop next-state 
                        next-char
                        (if act
                            act
                            longest-match-action)
                        next-length-bytes
                        (add1 length-chars)
                        (if act
                            length-chars
                            longest-match-length))]))]))
  (unless (input-port? ip)
    (raise-argument-error 'lexer "input-port?" 0 ip))
  (lexer ip))
      
(define (check-match lb first-pos longest-match-length length longest-match-action)
  (unless longest-match-action
    (define match (read-string length lb))
    (define end-pos (get-position lb))
    (raise-read-error
     (format "lexer: No match found in input starting with: ~v" match)
     (file-path)
     (position-line first-pos)
     (position-col first-pos)
     (position-offset first-pos)
     (- (position-offset end-pos) (position-offset first-pos))))
  (define match (read-string longest-match-length lb))
  ;(printf "(read-string ~e port) = ~e\n" longest-match-length match)
  (do-match lb first-pos longest-match-action match))

(define file-path (make-parameter #f))
(define lexer-file-path file-path)

(define (do-match ip first-pos action value)
  (action first-pos (get-position ip) value ip))
  
(define (get-position ip)
  (define-values (line col off) (port-next-location ip))
  (position off line col))


(define-lex-abbrev any-char (char-complement (union)))
(define-lex-abbrev any-string (intersection))
(define-lex-abbrev nothing (union))


(define-for-syntax (unicode-lex-abbrev range)
  (define/with-syntax ((lower upper) ...)
    (for/list ([range-component (in-list (force range))])
      (match-define (cons lower-int upper-int) range-component)
      (list (integer->char lower-int) (integer->char upper-int))))
  (lex-abbrev (λ () #'(union (char-range lower upper) ...))))


(define-syntax-parse-rule (define-unicode-abbrev name:id range:expr)
  (define-syntax name (unicode-lex-abbrev range)))

                                             
(define-unicode-abbrev alphabetic alphabetic-ranges)
(define-unicode-abbrev lower-case lower-case-ranges)
(define-unicode-abbrev upper-case upper-case-ranges)
(define-unicode-abbrev title-case title-case-ranges)
(define-unicode-abbrev numeric numeric-ranges)
(define-unicode-abbrev symbolic symbolic-ranges)
(define-unicode-abbrev punctuation punctuation-ranges)
(define-unicode-abbrev graphic graphic-ranges)
(define-unicode-abbrev whitespace whitespace-ranges)
(define-unicode-abbrev blank blank-ranges)
(define-unicode-abbrev iso-control iso-control-ranges)


(define-lex-trans (char-set stx)
  (syntax-parse stx
    [(_ STR:string)
     #:with (CHAR ...) (string->list (syntax-e #'STR))
     #'(union CHAR ...)]))


(define-for-syntax (make-lex-keyword-transformer id)
  (make-set!-transformer
   (λ (stx)
     (raise-syntax-error id "use of lexer keyword is not in an appropriate lexer action" stx))))

(define-syntax-rule (define-lex-keyword ID)
  (define-syntax-parameter ID (make-lex-keyword-transformer 'ID)))
  
(define-lex-keyword start-pos)
(define-lex-keyword end-pos)
(define-lex-keyword lexeme)
(define-lex-keyword lexeme-srcloc)
(define-lex-keyword input-port)
(define-lex-keyword return-without-pos)
(define-lex-keyword return-without-srcloc)
