# JANI Interface for Munta

This project provides a [JANI](https://jani-spec.org/) interface for the
verified model checker [Munta](https://github.com/wimmers/munta).
It is just a thin wrapper around the OCaml code that is generated for Munta.

## Installation

1. Install [opam](https://opam.ocaml.org/)
2. Install [symta](https://github.com/wimmers/symta) as an opam module
3. Install [Munta's `ocaml` branch](https://github.com/wimmers/munta/tree/ocaml)
   as an opam module (see section "OCaml" in README)
4. Clone this repository
5. Open it: `cd munta_jani`
6. Build: `dune build`

## Usage

The command format is:

```_build/default/bin/munta_jani.exe JANI_FILE```
