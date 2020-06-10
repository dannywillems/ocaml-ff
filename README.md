## OCaml FF

Play with Finite Field

This library provides 2 functors to instantiate finite field of arbitrary orders (in the limit of Zarith, the dependency to handle arbitrary integers).

```ocaml
module F13 = Ff.Make (struct let order = Z.of_string "13" end)
module F13_12 = Ff.MakeFp(struct let p = Z.of_string "13" let n = 12 end)
```

## JavaScript compatibility

This library can be transpiled in JavaScript using js_of_ocaml.
An example is provided in `js/test_js.ml`, with the corresponding `dune` file.
It instantiates Fp with p = 53. `dune` will compile this into a `FiniteField.js`
file, exporting methods like `add`, `toString`, `random`, etc. `FiniteField` can
be used as a Node module. See `js/test/test_js.js` for an example.

```
# Generate FiniteField.js
dune build js
cp _build/default/js/FiniteField.js ./
node
```

```js
var FF = require("./FiniteField.js");
let x = FF.random();
let y = FF.random();
let x_plus_y = FF.add(x, y);
```
