#lang racket/base
;; This module is copied and modified, with permission, from
;; https://github.com/sorawee/my-website/blob/master/coq-tactics/pollen.rkt
;; Copyright 2019, Sorawee Porncharoenwase and William J. Bowman.
(require
 racket/list
 racket/function
 racket/match
 racket/system
 racket/format
 racket/string
 "string-utils.rkt"
 threading)

(provide (all-defined-out))

(define current-coqtop-path (make-parameter (find-executable-path "coqtop")))

(define (analyze-coqtop-output output)
  (~> output
      (analyze/stream _ '())
      (drop-right _ 1) ;; the last one is always blank
      analyze/error
      (map (curryr string-join "\n") _)
      (map analyze/message _)
      (analyze/prev _ "")
      (map analyze/combine _)))

(define (analyze/stream xs acc-cmd)
  (match xs
    [(list) (list (reverse acc-cmd))]
    [(list fst rst ...)
     (cond
       [(string-prefix? fst "<prompt>")
        (define fst-truncated
          (regexp-replaces fst '([#px"<prompt>.*?</prompt>" ""])))
        (cons (reverse acc-cmd) (analyze/stream rst (list fst-truncated)))]
       [else (analyze/stream rst (cons fst acc-cmd))])]))

(define (analyze/error xs)
  (define-values (left right)
    (splitf-at xs (λ (lines) (not (string-prefix? (first lines) "Toplevel input,")))))
  (if (cons? right) (append left (list (first right))) left))

(define (analyze/message s)
  (define collected '())
  (define (collect _ matched)
    (set! collected (cons matched collected))
    "")
  (cons (regexp-replaces s (list (list #px"<infomsg>(.*?)</infomsg>\\s*" collect)))
        (string-join (reverse collected) "\n")))

(define (analyze/prev xs prev)
  (match xs
    [(list) '()]
    [(list (cons (pregexp #px"^\\s*$") info) rst ...)
     (cons (cons prev info) (analyze/prev rst prev))]
    [(list fst rst ...) (cons fst (analyze/prev rst (car fst)))]))

(define (analyze/combine pair) (string-append (car pair) "\n\n" (cdr pair)))

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

(define (read-coqtop-until-prompt in)
  (let loop ([output '()])
    ;; peak: if it's a prompt, coqtop is done
    (if (equal? (peek-string 8 0 in) "<prompt>")
        (reverse (cons (read-string-avail in) output))
        (match (read-line in)
          [(? string? v)
           (loop (cons v output))]
          [else (error "I wasn't expected a not-string, but found" else)]))))
