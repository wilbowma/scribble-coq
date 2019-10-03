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

## Caveats
When using `coq-example` uses a very primitive not-parser to figure out how many
commands to send to coqtop.
Every vernacular command must be "." followed by either a newline the end of the string.
