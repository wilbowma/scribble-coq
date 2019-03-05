#lang racket/base

(require
 scribble/minted
 scribble/base
 scribble/core
 racket/list
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
  (list
   (init-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err)
   (exit-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err)
   (eval-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err)))

(define interaction-prompt (make-element 'tt (list "> " )))

(define (coq-example #:eval ev . contents)
  ((first ev))
  (let ([example (apply string-append contents)])
    (list
     (nested
      #:style 'code-inset
      (para (list "Example(s):"))
      (nested
       interaction-prompt
       (coqinline example))
      (coqinline ((third ev) example))))))

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
