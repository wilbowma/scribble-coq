#lang racket/base

(require
 scribble/minted
 scribble/base
 scribble/core
 racket/list
 racket/string
 "coqtop-interop.rkt")

(provide (all-defined-out))

(define (coq . contents)
  (apply minted "coq" contents))

(define (coqinline . contents)
  (apply mintinline "coq" #:options '((lineseparator . "<br />")) contents))

;; A Coq evaluator is a list of an init-coqtop, exit-coqtop, and eval-coqtop
(define (make-coq-evaluator)
  ; NB: Bleh, I've reinvented objects, badly!
  (define-values (coqtop-proc coqtop-out coqtop-in coqtop-err)
    (values (box #f) (box #f) (box #f) (box #f)))
  ((init-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err))
  (list
   (init-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err)
   (exit-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err)
   (eval-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err)))

(define interaction-prompt (make-element 'tt (list "> " )))

(define (coq-example #:eval ev . contents)
  ;; I will not write a Coq parser
  (let ([examples (filter (λ (x) (not (equal? "" x))) (regexp-split #rx"(?m:\\.$)" (apply string-append contents)))])
    (list
     (para (list (if (> (length examples) 1) "Examples:" "Example:")))
     (apply nested
      #:style 'code-inset
      (for/list ([eg examples])
        (list
         (nested
          interaction-prompt
          (coqinline (string-append eg ".")))
         (coqinline ((third ev) (string-append eg ".")))))))))

(define (close-coq-eval ev)
  ((second ev)))

;; Blarg this doesn't work
#;(define (new-coq-evaluator)
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit 200]
                 [sandbox-eval-limits (list 5 20)]
                 [sandbox-path-permissions
                  (list
                   (list 'execute (current-sh-path))
                   (list 'execute (current-coqtop-path)))]
                 [sandbox-init-hook new-init-coqtop]
                 [sandbox-exit-handler new-exit-coqtop]
                 [sandbox-eval-handlers (list #f new-eval-coqtop)]
                 [current-print (lambda (x) (coq x))])
    ;; not actually racket/base; just needed something that can read strings.
    (make-evaluator 'racket/base)))
