#lang racket
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))

;function to scan for tokens
(define scan
  (lexer
   [(:: (:? #\w) (:+ #\r) (:+ #\i) (:+ #\t) (:+ #\e))
    (cons `WRITE
          (scan input-port))]

   [(:: (:? #\r) (:+ #\e) (:+ #\a) (:+ #\d))
    (cons `READ
          (scan input-port))]

   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    (cons `ID
          (scan input-port))]

   [(:: (:? #\:) (:+ #\=))
    (cons `ASGN
          (scan input-port))]
   
   {#\*
     (cons `MULDIVOP
          (scan input-port))}
   
   {(:or #\* #\/)
     (cons `MULDIVOP
          (scan input-port))}

   {(:: (:? #\$) (:+ #\$))
    (cons `EOF
          (scan input-port))}
   
   [#\( 
    (cons 'LPAR
          (scan input-port))]
   
   [#\)
    (cons 'RPAR
          (scan input-port))]
   
   [(:: (:? #\-) (:+ (char-range #\0 #\9)))
    (cons `INT
          (scan input-port))]
   
   [(:or #\+ #\-)
    (cons `ADDSUBOP
          (scan input-port))]
   
   [whitespace 
    (scan input-port)]
   
   [(eof)
    '()]))

;function to match expected tokens
(define (match expected token-list)
  (cond
    [(equal? expected 'EOF) (displayln "Accept Input")]
    [(equal? expected (first token-list)) (rest token-list)]
    [else displayln "Error: Input Not valid"]))

;function to parse input file
(define (parse token-list)
  (cond
    [(equal? 'ID (first token-list)) (parse (stmt_list token-list))]
    [(equal? 'READ (first token-list)) (parse (stmt_list token-list))]
    [(equal? 'WRITE (first token-list)) (parse (stmt_list token-list))]
    [(equal? 'EOF (first token-list)) (match (first token-list) 'EOF)]
    [else displayln "Error: Input Not valid"]))

(define (stmt_list token-list)
  (cond
   [(equal? 'ID (first token-list)) (stmt_list (stmt token-list))]
   [(equal? 'READ (first token-list)) (stmt_list (stmt token-list))]
   [(equal? 'WRITE (first token-list)) (stmt_list (stmt token-list))]
   [(equal? 'EOF (first token-list)) token-list]
   [else displayln "Error: Input Not valid"]))

(define (stmt token-list)
  (cond
  [(equal? 'ID (first token-list)) (expr (let ([x token-list]) (match 'ASGN (match 'ID x))))]
  [(equal? 'READ (first token-list)) ((match 'ID token-list (match 'READ token-list)))]
  [(equal? 'WRITE (first token-list)) (expr (match 'WRITE token-list))]
  [else displayln "Error: Input Not valid"]))

(define (expr token-list)
  (cond
  [(equal? 'ID (first token-list)) (term_tail (term token-list))]
  [(equal? 'INT (first token-list)) (term_tail (term token-list))]
  [(equal? 'LPAR (first token-list)) (term_tail (term token-list))]
  [else displayln "Error: Input Not valid"]))

(define (term_tail token-list)
  (cond
  [(equal? 'ADDSUBOP (first token-list)) (term_tail (term (add-sub-oper token-list)))]
  [(equal? 'RPAR (first token-list)) token-list]
  [(equal? 'ID (first token-list)) token-list]
  [(equal? 'READ (first token-list)) token-list]
  [(equal? 'WRITE (first token-list)) token-list]
  [(equal? 'EOF (first token-list)) token-list]
  [else displayln "Error: Input Not valid"]))

(define (term token-list)
  (cond
  [(equal? 'ID (first token-list)) (factor_tail (factor token-list))]
  [(equal? 'INT (first token-list)) (factor_tail (factor token-list))]
  [(equal? 'LPAR (first token-list)) (factor_tail (factor token-list))]
  [else displayln "Error: Input Not valid"]))

(define (factor_tail token-list)
  (cond
  [(equal? 'MULDIVOP (first token-list)) (factor_tail (factor (mul-div-oper token-list)))]
  [(equal? 'ADDSUBOP (first token-list)) token-list]
  [(equal? 'RPAR (first token-list)) token-list]
  [(equal? 'ID (first token-list)) token-list]
  [(equal? 'READ (first token-list)) token-list]
  [(equal? 'WRITE (first token-list)) token-list]
  [(equal? 'EOF (first token-list)) token-list]
  [else displayln "Error: Input Not valid"]))

(define (factor token-list)
  (cond
    [(equal? 'ID (first token-list)) (match 'ID token-list)]
    [(equal? 'INT (first token-list)) (match 'INT token-list)]
    [(equal? 'LPAR (first token-list)) (match 'RPAR (expr (match 'LPAR token-list)))]
    [else displayln "Error: Input Not valid"]))

(define (add-sub-oper token-list)
  (cond
    [(equal? 'ADDSUBOP (first token-list)) (match 'ADDSUBOP token-list)]
    [else displayln "Error: Input Not valid"]))

(define (mul-div-oper token-list)
  (cond
    [(equal? 'MULDIVOP (first token-list)) (match 'MULDIVOP token-list)]
    [else displayln "Error: Input Not valid"]))


;parse sample files
(parse (scan (open-input-file "input01.txt")))
;(parse (scan (open-input-file "input02.txt")))
;(parse (scan (open-input-file "input03.txt")))
;(parse (scan (open-input-file "input04.txt")))
;(parse (scan (open-input-file "input05.txt")))