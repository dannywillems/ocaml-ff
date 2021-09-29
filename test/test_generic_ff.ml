module F2 = Ff.MakeFp (struct
  let prime_order = Z.of_string "2"
end)

module F2Tests = Ff_pbt.MakeAllPrime (F2)

module F3 = Ff.MakeFp (struct
  let prime_order = Z.of_string "3"
end)

module F3SquareRoot = Ff_pbt.MakeSquareRoot (F3)
module F3Tests = Ff_pbt.MakeAllPrime (F3)
module F3RootOfUnity = Ff_pbt.MakeRootOfUnity (F3)

module F13 = Ff.MakeFp (struct
  let prime_order = Z.of_string "13"
end)

module F13SquareRootTests = Ff_pbt.MakeSquareRoot (F13)
module F13Tests = Ff_pbt.MakeAllPrime (F13)
module F13RootOfUnity = Ff_pbt.MakeRootOfUnity (F13)

module F1073740201 = Ff.MakeFp (struct
  let prime_order = Z.of_string "1073740201"
end)

module F1073740201SquareRootTests = Ff_pbt.MakeSquareRoot (F1073740201)
module F1073740201Tests = Ff_pbt.MakeAllPrime (F1073740201)
module F1073740201RootOfUnity = Ff_pbt.MakeRootOfUnity (F1073740201)

module FFLong = Ff.MakeFp (struct
  let prime_order =
    Z.of_string
      "4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787"
end)

module FFLongSquareRootTests = Ff_pbt.MakeSquareRoot (FFLong)
module FFLongTests = Ff_pbt.MakeAllPrime (FFLong)
module FFLongRootOfUnity = Ff_pbt.MakeRootOfUnity (FFLong)

(* module ScalarFieldBLS12_381Tests = Ff_pbt.MakeAll (ScalarFieldBLS12_381) *)

module ScalarFieldBLS12_381UnitTest_RootOfUnity = struct
  module ScalarFieldBLS12_381 = Ff.MakeFp (struct
    let prime_order =
      Z.of_string
        "52435875175126190479447740508185965837690552500527637822603658699938581184513"
  end)

  let test_vectors () =
    let vectors =
      [ ( "45578933624873246016802258050230213493140367389966312656957679049059636081617",
          1 lsl 16 );
        ( "15076889834420168339092859836519192632846122361203618639585008852351569017005",
          1 lsl 16 );
        ( "21584124886548760190346392867028830688912556631271990304491841940743921295609",
          1 lsl 32 );
        ( "27611812781829920551290133267575249478648871281233506899293410857719571783635",
          1 lsl 8 );
        ( "16624801632831727463500847948913128838752380757508923660793891075002624508302",
          1 lsl 4 ) ]
    in
    List.iter
      (fun (x, n) ->
        assert (
          ScalarFieldBLS12_381.is_nth_root_of_unity
            (Z.of_int n)
            (ScalarFieldBLS12_381.of_string x) ))
      vectors

  let get_tests () =
    let txt =
      "Test vectors for is_nth_root_of_unity for BLS12-381 scalar field"
    in
    let open Alcotest in
    (txt, [test_case "Test vectors" `Quick test_vectors])
end

(* This is the base field of the Curve 25519, the name comes from its order: p**255 - 19*)
module FFBaseCurve25519 = Ff.MakeFp (struct
  let prime_order = Z.(pow (succ one) 255 - of_int 19)
end)

module FFBaseCurve25519SquareRootTests = Ff_pbt.MakeSquareRoot (FFBaseCurve25519)
module FFBaseCurve25519Tests = Ff_pbt.MakeAllPrime (FFBaseCurve25519)

module F13_2 =
  Ff.MakeFp2
    (F13)
    (struct
      let nsr = F13.(negate (of_string "2"))
    end)

module F13_2Tests = Ff_pbt.MakeAll (F13_2)

let test_encoding_is_in_little_endian () =
  let open Alcotest in
  ( "Test to_bytes returns the little endian encoding",
    [ test_case "Test vectors with small Fp (13)" `Quick (fun () ->
          let module Fp = Ff.MakeFp (struct
            let prime_order = Z.of_string "13"
          end) in
          let test_vectors =
            [ ("1", [0b00000001]);
              ("0", [0b00000000]);
              ("13", [0b00000000]);
              ("14", [0b00000001]) ]
          in
          List.iter
            (fun (v, expected_bytes) ->
              let expected_bytes =
                Bytes.init (List.length expected_bytes) (fun i ->
                    char_of_int (List.nth expected_bytes i))
              in
              assert (Bytes.equal Fp.(to_bytes (of_string v)) expected_bytes))
            test_vectors);
      test_case "Test vectors with Fp on two bytes (509)" `Quick (fun () ->
          let module Fp = Ff.MakeFp (struct
            let prime_order = Z.of_string "509"
          end) in
          let test_vectors =
            [ ("1", [0b00000001; 0b00000000]);
              ("0", [0b00000000; 0b00000000]);
              ("13", [0b00001101; 0b00000000]);
              ("509", [0b00000000; 0b00000000]) ]
          in
          List.iter
            (fun (v, expected_bytes) ->
              let expected_bytes =
                Bytes.init (List.length expected_bytes) (fun i ->
                    char_of_int (List.nth expected_bytes i))
              in
              assert (Bytes.equal Fp.(to_bytes (of_string v)) expected_bytes))
            test_vectors) ] )

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

let test_vectors_legendre_symbol () =
  let open Alcotest in
  (* Table of value here: https://en.wikipedia.org/wiki/Legendre_symbol *)
  let test_vectors =
    [ ("5", "2", "-1");
      ("17", "1", "1");
      ("17", "2", "1");
      ("17", "3", "-1");
      ("17", "4", "1");
      ("17", "5", "-1");
      ("17", "6", "-1");
      ("17", "7", "-1");
      ("17", "8", "1");
      ("2", "1", "1");
      ("3", "1", "1");
      ("3", "2", "-1");
      ("5", "1", "1");
      ("7", "1", "1");
      ("97", "1", "1");
      ("101", "1", "1");
      ("103", "1", "1");
      ("103", "17", "1");
      ("103", "21", "-1");
      ("103", "22", "-1");
      ("127", "1", "1") ]
  in
  ( "Test legendre symbol result on test vectors",
    [ test_case "Test vectors" `Quick (fun () ->
          List.iter
            (fun (prime_order, v, expected_result) ->
              let module Fp = Ff.MakeFp (struct
                let prime_order = Z.of_string prime_order
              end) in
              assert (
                Z.equal
                  (Fp.legendre_symbol (Fp.of_string v))
                  (Z.of_string expected_result) ))
            test_vectors) ] )

let test_vectors_power_of_two () =
  let open Alcotest in
  (* Table of value here: https://en.wikipedia.org/wiki/Legendre_symbol *)
  let test_vectors =
    [ ("7", (1, "3"));
      ("13", (2, "3"));
      ("127", (1, "63"));
      ("90859051", (1, "45429525")) ]
  in
  ( "Test factor in power of two (used for instance for Tonelli Shanks)",
    [ test_case "Test vectors" `Quick (fun () ->
          List.iter
            (fun (prime_order, (expected_s, expected_q)) ->
              let module Fp = Ff.MakeFp (struct
                let prime_order = Z.of_string prime_order
              end) in
              let (s, q) = Fp.factor_power_of_two in
              assert (s = expected_s && Z.equal q (Z.of_string expected_q)) ;
              let res = Z.(mul (pow (Z.succ Z.one) s) q) in
              assert (Z.(equal res (Z.pred (Z.of_string prime_order)))))
            test_vectors) ] )

let () =
  let open Alcotest in
  run
    "Random fields"
    ( test_size_in_bytes ()
      :: test_encoding_is_in_little_endian ()
      :: test_vectors_legendre_symbol ()
      :: test_vectors_power_of_two ()
      :: F3SquareRoot.get_tests () :: F3RootOfUnity.get_tests ()
      :: F13SquareRootTests.get_tests ()
      :: F13RootOfUnity.get_tests ()
      :: F1073740201SquareRootTests.get_tests ()
      :: F1073740201RootOfUnity.get_tests ()
      :: FFLongRootOfUnity.get_tests ()
      :: FFLongSquareRootTests.get_tests ()
      :: FFBaseCurve25519SquareRootTests.get_tests ()
      :: ScalarFieldBLS12_381UnitTest_RootOfUnity.get_tests ()
      :: F2Tests.get_tests ()
    @ F3Tests.get_tests () @ F13Tests.get_tests () @ FFLongTests.get_tests ()
    @ FFBaseCurve25519Tests.get_tests ()
    @ F13_2Tests.get_tests () )
