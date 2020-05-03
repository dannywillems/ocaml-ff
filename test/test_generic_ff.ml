module F13 = Ff.Make (struct
  let order = Z.of_string "13"
end)

module F10000004089 = Ff.Make (struct
  let order = Z.of_string "1073740201"
end)

module F13ValueGeneration = Test_ff_make.MakeValueGeneration (F13)
module F13IsZero = Test_ff_make.MakeIsZero (F13)
module F13Equality = Test_ff_make.MakeEquality (F13)
module F13FieldProperties = Test_ff_make.MakeFieldProperties (F13)
module FF1073740201ValueGeneration =
  Test_ff_make.MakeValueGeneration (F10000004089)
module FF1073740201IsZero = Test_ff_make.MakeIsZero (F10000004089)
module FF1073740201Equality = Test_ff_make.MakeEquality (F10000004089)
module FF1073740201FieldProperties =
  Test_ff_make.MakeFieldProperties (F10000004089)

let () =
  let open Alcotest in
  run
    "Random fields"
    [ F13IsZero.get_tests ();
      F13ValueGeneration.get_tests ();
      F13Equality.get_tests ();
      F13FieldProperties.get_tests ();
      FF1073740201IsZero.get_tests ();
      FF1073740201ValueGeneration.get_tests ();
      FF1073740201Equality.get_tests ();
      FF1073740201FieldProperties.get_tests () ]
