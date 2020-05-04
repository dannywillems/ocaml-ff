## OCaml FF

Play with Finite Field

This library provides 2 functors to instantiate finite field of arbitrary orders (in the limit of Zarith, the dependency to handle arbitrary integers).

```ocaml
module F13 = Ff.Make (struct let order = Z.of_string "13" end)
module F13_12 = Ff.MakeFp(struct let p = Z.of_string "13" let n = 12 end)
```
