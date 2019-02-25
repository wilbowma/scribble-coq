#lang racket/base
;; This module is copied and modified, with permission, from
;; https://github.com/sorawee/my-website/blob/master/coq-tactics/pollen.rkt
(require
 racket/sequence
 racket/list
 racket/function
 racket/match
 racket/system
 racket/format
 racket/string
 racket/set
 threading)

(provide (all-defined-out))

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

(define (call-coq/core src)
  (match-define (list in out _ err proc)
    (process (~a (find-executable-path "coqtop") " -emacs 2>&1")))
  (display src out)
  (close-output-port out)
  (proc 'wait)
  (define output '())
  (let loop ()
    (match (read-line in)
      [(? string? v) (set! output (cons v output))
                     (loop)]
      [_ (void)]))
  (close-input-port in)
  (close-input-port err)
  (proc 'kill)
  (rest (reverse output)))
