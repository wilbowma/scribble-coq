# scribble/coq

A small Scribble library for type setting Coq code and coqtop interactions.

I have not thoroughly tested this.

## Example

See `example.scrbl`

Run `scribble`, with multiple backends.

`scribble --pdf example.scrbl`
`scribble --html example.scrbl`

See pretty code!

## Install
`raco pkg install scribble-coq`

Requires `scribble-minted`.

You must have `pygmentize` and `coqtop` installed and in your `PATH`.
