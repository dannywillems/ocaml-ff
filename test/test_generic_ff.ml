module F13 = Ff.MakeFp (struct
  let prime_order = Z.of_string "13"
end)

module F1073740201 = Ff.MakeFp (struct
  let prime_order = Z.of_string "1073740201"
end)

module F13ValueGeneration = Test_ff_make.MakeValueGeneration (F13)
module F13IsZero = Test_ff_make.MakeIsZero (F13)
module F13Equality = Test_ff_make.MakeEquality (F13)
module F13FieldProperties = Test_ff_make.MakeFieldProperties (F13)
module FF1073740201ValueGeneration =
  Test_ff_make.MakeValueGeneration (F1073740201)
module FF1073740201IsZero = Test_ff_make.MakeIsZero (F1073740201)
module FF1073740201Equality = Test_ff_make.MakeEquality (F1073740201)
module FF1073740201FieldProperties =
  Test_ff_make.MakeFieldProperties (F1073740201)

module FFLong = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"
end)

module FFLongValueGeneration = Test_ff_make.MakeValueGeneration (FFLong)
module FFLongIsZero = Test_ff_make.MakeIsZero (FFLong)
module FFLongEquality = Test_ff_make.MakeEquality (FFLong)
module FFLongFieldProperties = Test_ff_make.MakeFieldProperties (FFLong)

let test_size_in_bytes () =
  let open Alcotest in
  ( "Test size in bytes computation for different orders",
    [ test_case "Test vectors" `Quick (fun () ->
          let l =
            [ ("2", 1);
              ("13", 1);
              ("257", 2);
              ("1024", 2);
              ("65536", 3);
              ( "52435875175126190479447740508185965837690552500527637822603658699938581184513",
                32 );
              ( "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787",
                48 ) ]
          in
          List.iter
            (fun (order, expected_nb_bytes) ->
              let module P = Ff.MakeFp (struct
                let prime_order = Z.of_string order
              end) in
              print_int P.size_in_bytes ;
              assert (P.size_in_bytes = expected_nb_bytes))
            l) ] )

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
      FF1073740201FieldProperties.get_tests ();
      FFLongIsZero.get_tests ();
      FFLongValueGeneration.get_tests ();
      FFLongEquality.get_tests ();
      FFLongFieldProperties.get_tests ();
      test_size_in_bytes () ]
