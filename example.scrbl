#lang scribble/base

@(require scribble/coq)

@(define coq-evaluator (make-coq-evaluator))

The following example defines the natural numbers.
@coq-example[#:eval coq-evaluator]{
Inductive Nat : Set :=
| z : Nat
| s : Nat -> Nat.}

@(close-coq-eval coq-evaluator)