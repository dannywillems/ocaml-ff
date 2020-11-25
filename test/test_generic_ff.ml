module F2 = Ff.MakeFp (struct
  let prime_order = Z.of_string "2"
end)

module F2Tests = Ff_pbt.MakeAll (F2)

module F13 = Ff.MakeFp (struct
  let prime_order = Z.of_string "13"
end)

module F13Tests = Ff_pbt.MakeAll (F13)

module F1073740201 = Ff.MakeFp (struct
  let prime_order = Z.of_string "1073740201"
end)

module F1073740201Tests = Ff_pbt.MakeAll (F1073740201)

module FFLong = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"
end)

module FFLongTests = Ff_pbt.MakeAll (FFLong)

(* This is the base field of the Curve 25519, the name comes from its order: p**255 - 19*)
module FFBaseCurve25519 = Ff.MakeFp (struct
  let prime_order = Z.(pow (succ one) 255 - of_int 19)
end)

module FFBaseCurve25519Tests = Ff_pbt.MakeAll (FFBaseCurve25519)

module F13_2 =
  Ff.MakeFp2
    (F13)
    (struct
      let nsr = F13.(negate (of_string "2"))
    end)

module F13_2Tests = Ff_pbt.MakeAll (F13_2)

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
              assert (P.size_in_bytes = expected_nb_bytes))
            l) ] )

let () =
  let open Alcotest in
  run
    "Random fields"
    ( (test_size_in_bytes () :: F2Tests.get_tests ())
    @ F13Tests.get_tests () @ FFLongTests.get_tests ()
    @ FFBaseCurve25519Tests.get_tests ()
    @ F13_2Tests.get_tests () )
