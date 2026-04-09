# OCaml FF

Finite field arithmetic in OCaml, built on
[Zarith](https://github.com/ocaml/Zarith) for arbitrary-precision integers.

## Features

- Prime fields GF(p) via the `Ff.MakeFp` functor
- Quadratic extensions GF(p^2) via the `Ff.MakeFp2` functor
- Tonelli-Shanks square root computation
- Roots of unity
- Property-based testing helpers (`ff-pbt`)
- Optional js_of_ocaml support

## Packages

| Package | Description |
|---------|-------------|
| `ff-sig` | Module type signatures for finite fields |
| `ff` | Functor implementations (`MakeFp`, `MakeFp2`) |
| `ff-pbt` | Property-based testing for any `Ff_sig.BASE` implementation |

## Install

```
opam install ff
```

## Quick start

```ocaml
(* Prime field with p = 13 *)
module F13 = Ff.MakeFp (struct
  let prime_order = Z.of_string "13"
end)

(* BLS12-381 scalar field *)
module BLS_Fr = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "52435875175126190479447740508185965837690552500527637822603658699938581184513"
end)

let () =
  let x = F13.random () in
  let y = F13.random () in
  let sum = F13.add x y in
  Printf.printf "%s + %s = %s\n"
    (F13.to_string x) (F13.to_string y) (F13.to_string sum)
```

Infix operators are available:

```ocaml
let open F13 in
let x = random () in
let y = random () in
let z = (x + y) * x in
assert (z = (x * x) + (y * x))
```

## Quadratic extensions

```ocaml
module Fp = Ff.MakeFp (struct
  let prime_order = Z.of_string "59"
end)

(* GF(59^2) = Fp[X] / (X^2 - nsr) *)
module Fp2 = Ff.MakeFp2 (Fp) (struct
  let nsr = Fp.of_int 5
end)
```

## Property-based testing

The `ff-pbt` package provides generic property tests for any
field implementing `Ff_sig.BASE`:

```ocaml
module MyFieldProperties = Ff_pbt.MakeFieldProperties (MyField)

let () =
  let open Alcotest in
  run "MyField" [MyFieldProperties.get_tests ()]
```

## JavaScript support

This library can be compiled to JavaScript using js_of_ocaml.
See `js/test_js.ml` for an example. Build with:

```
dune build @js/runtest-js
```

## Documentation

API documentation is available at
https://dannywillems.github.io/ocaml-ff/

To generate locally:

```
dune build @doc
```

Then open `_build/default/_doc/_html/index.html`.

## Development

```
# Install dependencies
opam install . --deps-only --with-test

# Build
dune build

# Run tests
dune runtest

# Format code
dune fmt
```

## License

MIT
