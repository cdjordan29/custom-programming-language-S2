;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname S2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; S2.rkt
;;; @see http://www.radford.edu/~itec380/2017fall-ibarland/Homeworks/S2.html
;;; @author Daniel Jordan
;;; @version 2017-Nov-28

;;; Test cases are in: expr-test-S2.rkt

(require "student-extras.rkt")
(require "scanner.rkt")
(provide (all-defined-out))
#|

  Expr   ::= Num | Paren | BinOp | Ifnz | Ifgt | Id | LetExpr   ;>>>S1,S2
  Paren  ::= | Expr |          (note: these “|”s are literal -- not BNF “or”s)
  BinOp  ::= ? Op Expr Expr
  Ifnz   ::= ( Expr ) !0  Expr : Expr 
  Op     ::= ad | su | mu |mo                ;>>>S1
  Ifgt   ::=  (Expr >? Expr) = Expr : Expr   ;>>>S1
  LetExpr ::= let Id Expr in Expr            ;>>>S2
  FuncExpr ::= func Id -> Expr               ;>>>S3
  FuncApplyExpr ::= call Expr (Expr)         ;>>>S3
|#

; An Expr is:
;  - a number
;  - (make-paren [Expr])
;  - (make-binop [Op] [Expr] [Expr])
;  - (make-ifnz  [Expr] [Expr] [Expr])
;  - (make-ifgt  [Expr] [Expr] [Expr] [Expr])
;  - (make-let-expr [Id] [Expr] [Expr])
;  - (make-func-expr [Id][Expr])             ;>>>S3
;  - (make-func-app-expr [Expr] [Expr])    ;>>>S3

(define OPS (list "ad" "su" "mu" "mo"))      ;>>>S1
; An Op is: (one-of OPS)

(define-struct binop (op left right))
(define-struct paren (e))
(define-struct ifnz  (cond nz-branch z-branch))
(define-struct ifgt  (comp1 comp2 gt-branch le-branch))  ;>>>S1
; An Id is: a string                         ;>>>S2
(define-struct let-expr (id rhs body))       ;>>>S2
(define-struct func-expr (id expr))
(define-struct func-app-expr (lhs rhs))


; Examples of Expr:
34
(make-paren 34)
(make-binop "ad" 3 4)
(make-binop "ad" (make-paren 34)  (make-binop "mu" 3 4))
(make-ifnz 3 7 9)
(make-ifnz (make-paren 1)
           (make-binop "ad" (make-paren 34)  (make-binop "mu" 3 4))
           (make-ifnz 0 7 9))






; parse! : (scanner OR string) -> Expr
; given a scanner, consume one S0 expression off the front of it
; and
; return the corresponding parse-tree.
;
(define (parse! s)
  ; We use recursive-descent parsing.
  (cond [(string? s) (parse! (create-scanner s))]   ; overload: scanner *or* string.
        [(number? (peek s)) (pop! s)]
        [(string=? "|" (peek s))
         (let* {[_ (pop! s)]
                [the-inside-expr (parse! s)]
                [_ (pop! s)]  ; the closing-pipe
                }
           (make-paren the-inside-expr))]
        [(string=? "?" (peek s))
         (let* {[question-mark (pop! s)]
                [_ (if (not (member? (peek s) OPS)) (error 'parse "Unknown op ~v" (peek s)) 'carry-on)]
                [op     (pop! s)]
                [lefty  (parse! s)]
                [righty (parse! s)]
                }
           (make-binop op lefty righty))]
        [(string=? "(" (peek s))
         (let* {[_ (pop! s)]   ; throw away the opening "("
                [the-cond-or-comp1 (parse! s)]
                }
           (cond [(string=? ")" (peek s))                ;>>>S1 decide if we have ifnz or ifgt
                  (let* {[_ (pop! s)]   ; discard ")"
                         [_ (pop! s)]   ; discard "!"
                         [_ (pop! s)]   ; discard "0"
                         [the-nz-branch (parse! s)]
                         [_ (pop! s)] ; throw away the ":"
                         [the-z-branch  (parse! s)]
                         }
                    (make-ifnz the-cond-or-comp1 the-nz-branch the-z-branch))]
                 [(string=? ">" (peek s))                ;>>>S1 handle ifgt
                  (let* {[_ (pop! s)]   ; discard ">"  
                         [_ (pop! s)]   ; discard "?"
                         [the-comp2 (parse! s)] 
                         [_ (pop! s)] ; discard ")"       
                         [_ (pop! s)] ; discard "="  
                         [the-gt-branch (parse! s)]
                         [_ (pop! s)] ; discard ":" 
                         [the-le-branch  (parse! s)]       
                         }
                    (make-ifgt the-cond-or-comp1 the-comp2 the-gt-branch the-le-branch))]))]
        [(string=? (peek s) "let")    ;>>>S2 let
         (let* {[_ (pop! s)] ; discard "let"  
                [the-id (pop! s)]
                [the-rhs (parse! s)] 
                [_ (pop! s)] ; discard "in"       
                [the-body (parse! s)]       
                }
           (make-let-expr the-id the-rhs the-body))]
        [(string? (peek s)) (pop! s)] ;>>>S2 Id
        [else (error 'parse! "syntax error -- something has gone awry!  Seeing \"" (peek s) "\"")]))




; eval : Expr -> Num
; Return the value which this Expr evaluates to.
; In S0, the only type of value is a Num.
;
(define (eval e)
  (cond [(number? e) e]
        [(paren? e) (eval (paren-e e))]  
        [(binop? e) (let* {[the-op (binop-op e)]
                           [left-val (eval (binop-left e))] 
                           [right-val (eval (binop-right e))]}       
                      (cond [(string=? "ad" the-op) (+ left-val right-val)]
                            [(string=? "su" the-op) (- left-val right-val)]
                            [(string=? "mu" the-op) (* left-val right-val)]
                            [(string=? "mo" the-op) (let {[q (/ left-val right-val)]}
                                                      (* right-val (- q (floor q))))]
                            [else (error 'eval "unimplemented operator " the-op)]))]
        [(ifnz? e) (if (not (zero? (eval (ifnz-cond e))))
                       (eval (ifnz-nz-branch e))
                       (eval (ifnz-z-branch e)))]
        [(ifgt? e) (if (> (eval (ifgt-comp1 e)) (eval (ifgt-comp2 e)))
                       (eval (ifgt-gt-branch e))
                       (eval (ifgt-le-branch e)))]
        [(let-expr? e) (let* {[v0 (eval (let-expr-rhs e))]  
                              [e′ (subst (let-expr-id e) v0 (let-expr-body e))] 
                              }       
                         (eval e′))]
        [(string? e) (error 'eval "unbound identifier: " e)]
        [else (error 'eval "unknown type of expr " (expr->string e))]))


              

; expr->string : Expr -> string
; Return a string-representation of `e`.
;
(define (expr->string e)
  (cond [(number? e) (number->string (if (integer? e) e (exact->inexact e)))]
        [(paren? e) (string-append "|" (expr->string (paren-e e)) "|")]
        [(binop? e) (string-append "?"
                                   (binop-op e)
                                   " "
                                   (expr->string (binop-left e))
                                   " "
                                   (expr->string (binop-right e))
                                   )]
        [(ifnz? e) (string-append "("
                                   (expr->string (ifnz-cond e))
                                   ") "
                                   "!0 "
                                   (expr->string (ifnz-nz-branch e))
                                   " : "
                                   (expr->string (ifnz-z-branch e))
                                   )]
        [(ifgt? e) (string-append "("                                     ;>>>S1 ifgt
                                   (expr->string (ifgt-comp1 e))
                                   " >? "
                                   (expr->string (ifgt-comp2 e))
                                   ") = "
                                   (expr->string (ifgt-gt-branch e))
                                   " : "
                                   (expr->string (ifgt-le-branch e))
                                   )]
        [(string? e) e] ;>>>S2 Id
        [(let-expr? e) (string-append "let "                               ;>>> S2 let
                                      (let-expr-id e)
                                      " "
                                      (expr->string (let-expr-rhs e))
                                      " in "
                                      (expr->string (let-expr-body e))
                                      )]
        [else (error 'expr->string (format "unknown type of expr: " e))]))

; subst : Id, Num, Expr -> Expr           ;>>>S2
; Return an Expr just like `e`, except each occurrence of `id` is replaced with `v`.
;
(define (subst id v e)
  (cond [(number? e) e]
        [(paren? e) (make-paren (subst id v (paren-e e)))]
        [(binop? e) (make-binop (binop-op e)
                                (subst id v (binop-left e))
                                (subst id v (binop-right e)))]
        [(ifnz? e) (make-ifnz (subst id v (ifnz-cond e))
                              (subst id v (ifnz-nz-branch e))
                              (subst id v (ifnz-z-branch e)))]
        [(ifgt? e) (make-ifgt (subst id v (ifgt-comp1 e))
                              (subst id v (ifgt-comp2 e))
                              (subst id v (ifgt-gt-branch e))
                              (subst id v (ifgt-le-branch e)))]
        [(string? e)   (if (string=? id e) v e)]  ; the one interesting case
        [(let-expr? e) (if (string=? (let-expr-id e) id) ;>>>>S3
                           (make-let-expr (let-expr-id e) ; make letexpr
                                          (let-expr-rhs e)
                                          (let-expr-body e))
                                 (make-let-expr (let-expr-id e) ; continue making letexpr with sub
                                      (subst id v (let-expr-rhs e))
                                      (subst id v (let-expr-body e))))]
        [else (error 'subst (format "unknown type of expr: " e))]))

;>>>S2 tests for 'subst'
(require rackunit)
(check-equal? (subst "x" 9 (parse! "3"))   (parse! "3") )
(check-equal? (subst "x" 9 (parse! "x"))   (parse! "9") )
(check-equal? (subst "z" 7 (parse! "x"))   (parse! "x") )
(check-equal? (subst "z" 7 (parse! "?ad 4 z"))   (parse! "?ad 4 7") )
(check-equal? (subst "z" 7 (parse! "let x z in ?mu x z"))
              (parse! "let x 7 in ?mu x 7"))

