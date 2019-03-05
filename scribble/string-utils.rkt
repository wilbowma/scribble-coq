#lang racket/base

(provide (all-defined-out))

(define (read-string-avail in)
  (let loop ([chars '()])
    (if (char-ready? in)
        (loop (cons (read-char in) chars))
        (apply string (reverse chars)))))
