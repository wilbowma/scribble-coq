#lang racket/base

(require
 scribble/minted
 scribble/base
 scribble/core
 racket/sandbox
 racket/match
 racket/system
 racket/format
 racket/string
 racket/list
 racket/port
 "coqtop-interop.rkt")

(provide (all-defined-out))

(define (coq . contents)
  (apply minted "coq" contents))

(define (coqinline . contents)
  (apply mintinline "coq" #:options '((lineseparator . "<br />")) contents))

(define current-coqtop-path (make-parameter (find-executable-path "coqtop")))
(define current-sh-path (make-parameter (find-executable-path "sh")))

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
  (define outputs (map (lambda (x)
                         (list
                          (nested
                           (para (list "Example(s):"))
                           (nested
                            interaction-prompt
                            (para (coqinline x)))
                           (coqinline ((third ev) x)))))
                       contents))
  (apply nested #:style 'code-inset outputs))

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

(define meow (current-output-port))
;; Code of next three functions largely due to sorawee, except for all the
;; boxing/unbox that I added.

(define (read-string-avail in)
  (let loop ([chars '()])
    (if (char-ready? in)
        (loop (cons (read-char in) chars))
        (apply string (reverse chars)))))

(define (read-coqtop-until-prompt in)
  (let loop ([output '()])
    (displayln output meow)
    ;; peak: if it's a prompt, coqtop is done
    (if (equal? (peek-string 8 0 in) "<prompt>")
        (begin
          (displayln "done" meow)
          (reverse (cons (read-string-avail in) output)))
        (match (read-line in)
          [(? string? v)
           (loop (cons v output))]
          [else (error "I wasn't expected a not-string, but found" else)]))))

;; Code below is based on call-coq/core from coqtop-interop, but I've separated
;; out so I can make repeated calls to the same coqtop process.
(define ((init-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err))
  (match-define (list in out _ err proc)
    (process (~a (current-coqtop-path) " -emacs 2>&1")))
  (set-box! coqtop-proc proc)
  (set-box! coqtop-out out)
  (set-box! coqtop-in in)
  (set-box! coqtop-err err)
  (read-coqtop-until-prompt in))

(define ((eval-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err) str)
  (displayln str (unbox coqtop-out))
  (flush-output (unbox coqtop-out))
  (string-append* (analyze-coqtop-output (read-coqtop-until-prompt (unbox coqtop-in)))))

(define ((exit-coqtop coqtop-proc coqtop-out coqtop-in coqtop-err))
  (close-output-port (unbox coqtop-out))
  (close-input-port (unbox coqtop-in))
  (close-input-port (unbox coqtop-err))
  ((unbox coqtop-proc) 'kill))
