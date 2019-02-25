#lang scribble/base

@(require pict scribble/minted "coq.rkt")

@(define coq-evaluator (make-coq-evaluator))
@coq-example[#:eval coq-evaluator
          "Inductive Nat : Set :=
           | z : Nat
           | s : Nat -> Nat."]

@(close-coq-eval coq-evaluator)