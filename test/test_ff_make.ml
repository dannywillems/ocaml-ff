let rec repeat n f =
  if n <= 0 then
    let f () = () in
    f
  else (
    f () ;
    repeat (n - 1) f )

(** Check the routine generators do not raise any exception *)
module MakeValueGeneration (FiniteField : Ff.T) = struct
  let zero () = ignore @@ FiniteField.zero ()

  let random () = ignore @@ FiniteField.random ()

  let one () = ignore @@ FiniteField.one ()

  let rec inverse_with_random_not_null () =
    let random = FiniteField.random () in
    if FiniteField.is_zero random then inverse_with_random_not_null ()
    else ignore @@ FiniteField.inverse random

  let inverse_with_one () =
    let one = FiniteField.one () in
    ignore @@ FiniteField.inverse one

  let negation_with_random () =
    let random = FiniteField.random () in
    ignore @@ FiniteField.negate random

  let negation_with_zero () =
    let zero = FiniteField.zero () in
    ignore @@ FiniteField.negate zero

  let negation_with_one () =
    let one = FiniteField.one () in
    ignore @@ FiniteField.negate one

  let square_with_one () =
    let one = FiniteField.one () in
    ignore @@ FiniteField.square one

  let square_with_random () =
    let g = FiniteField.random () in
    ignore @@ FiniteField.square g

  let double_with_zero () =
    let g = FiniteField.zero () in
    ignore @@ FiniteField.double g

  let double_with_one () =
    let one = FiniteField.one () in
    ignore @@ FiniteField.double one

  let double_with_random () =
    let g = FiniteField.random () in
    ignore @@ FiniteField.double g

  let double_is_same_than_multiply_by_same_element () =
    let g = FiniteField.random () in
    assert (FiniteField.eq (FiniteField.double g) (FiniteField.add g g))

  let get_tests () =
    let open Alcotest in
    ( "value generation",
      [ test_case "zero" `Quick (repeat 1000 zero);
        test_case "random" `Quick (repeat 1000 random);
        test_case
          "inverse_random_not_null"
          `Quick
          (repeat 1000 inverse_with_random_not_null);
        test_case "negate_with_one" `Quick (repeat 1000 negation_with_one);
        test_case "negate_with_zero" `Quick (repeat 1000 negation_with_zero);
        test_case "double_with_one" `Quick (repeat 1000 double_with_one);
        test_case "double_with_zero" `Quick (repeat 1000 double_with_zero);
        test_case "double_with_random" `Quick (repeat 1000 double_with_random);
        test_case "square_with_one" `Quick (repeat 1000 square_with_one);
        test_case "square_with_random" `Quick (repeat 1000 square_with_random);
        test_case
          "negate_with_random"
          `Quick
          (repeat 1000 negation_with_random);
        test_case
          "double_is_same_than_multiply_by_same_element"
          `Quick
          (repeat 1000 double_is_same_than_multiply_by_same_element);
        test_case "inverse_one" `Quick (repeat 1000 inverse_with_one) ] )
end

module MakeIsZero (FiniteField : Ff.T) = struct
  let with_zero_value () =
    assert (FiniteField.is_zero (FiniteField.zero ()) = true)

  let with_random_value () =
    assert (FiniteField.is_zero (FiniteField.random ()) = false)

  let get_tests () =
    let open Alcotest in
    ( "is_zero",
      [ test_case "with zero value" `Quick (repeat 1000 with_zero_value);
        test_case "with random value" `Quick (repeat 1000 with_random_value) ]
    )
end

module MakeEquality (FiniteField : Ff.T) = struct
  let zero_two_different_objects () =
    assert (FiniteField.eq (FiniteField.zero ()) (FiniteField.zero ()))

  let zero_same_objects () =
    let zero = FiniteField.zero () in
    assert (FiniteField.eq zero zero)

  let one_two_different_objects () =
    assert (FiniteField.eq (FiniteField.one ()) (FiniteField.one ()))

  let one_same_objects () =
    let one = FiniteField.one () in
    assert (FiniteField.eq one one)

  let random_same_objects () =
    let random = FiniteField.random () in
    assert (FiniteField.eq random random)

  let get_tests () =
    let open Alcotest in
    ( "equality",
      [ test_case
          "zero_two_different_objects"
          `Quick
          (repeat 1000 zero_two_different_objects);
        test_case "zero_same_objects" `Quick (repeat 1000 zero_same_objects);
        test_case
          "one_two_different_objects"
          `Quick
          (repeat 1000 one_two_different_objects);
        test_case "one_same_objects" `Quick (repeat 1000 one_same_objects);
        test_case
          "random_same_objects"
          `Quick
          (repeat 1000 random_same_objects) ] )
end

module MakeFieldProperties (FiniteField : Ff.T) = struct
  let zero_nullifier_random () =
    (* 0 * g = 0 *)
    let zero = FiniteField.zero () in
    let random = FiniteField.random () in
    assert (FiniteField.is_zero (FiniteField.mul zero random))

  let zero_nullifier_zero () =
    (* Special case 0 * 0 = 0 *)
    let zero = FiniteField.zero () in
    assert (FiniteField.is_zero (FiniteField.mul zero zero))

  let zero_nullifier_one () =
    (* Special case 0 * 1 = 0 *)
    let zero = FiniteField.zero () in
    let one = FiniteField.one () in
    assert (FiniteField.is_zero (FiniteField.mul zero one))

  let inverse_of_one_is_one () =
    let one = FiniteField.one () in
    assert (FiniteField.eq (FiniteField.inverse one) one)

  let zero_has_no_inverse () =
    let zero = FiniteField.zero () in
    match FiniteField.inverse_opt zero with
    | Some _ ->
        assert false
    | None ->
        assert true

  let rec inverse_of_non_null_does_exist () =
    let random = FiniteField.random () in
    if FiniteField.is_zero random then inverse_of_non_null_does_exist ()
    else
      match FiniteField.inverse_opt random with
      | Some _ ->
          assert true
      | None ->
          assert false

  let rec inverse_of_inverse () =
    let random = FiniteField.random () in
    if FiniteField.is_zero random then inverse_of_inverse ()
    else
      assert (
        FiniteField.eq
          (FiniteField.inverse (FiniteField.inverse random))
          random )

  let opposite_of_opposite () =
    let random = FiniteField.random () in
    assert (
      FiniteField.eq (FiniteField.negate (FiniteField.negate random)) random )

  let opposite_of_zero_is_zero () =
    let zero = FiniteField.zero () in
    assert (FiniteField.eq (FiniteField.negate zero) zero)

  let additive_associativity () =
    let g1 = FiniteField.random () in
    let g2 = FiniteField.random () in
    let g3 = FiniteField.random () in
    assert (
      FiniteField.eq
        (FiniteField.add (FiniteField.add g1 g2) g3)
        (FiniteField.add (FiniteField.add g2 g3) g1) )

  let distributivity () =
    let g1 = FiniteField.random () in
    let g2 = FiniteField.random () in
    let g3 = FiniteField.random () in
    assert (
      FiniteField.eq
        (FiniteField.mul (FiniteField.add g1 g2) g3)
        (FiniteField.add (FiniteField.mul g1 g3) (FiniteField.mul g2 g3)) )

  let multiplicative_associativity () =
    let g1 = FiniteField.random () in
    let g2 = FiniteField.random () in
    let g3 = FiniteField.random () in
    assert (
      FiniteField.eq
        (FiniteField.mul (FiniteField.mul g1 g2) g3)
        (FiniteField.mul (FiniteField.mul g2 g3) g1) )

  (** 0**0 = 1 *)
  let pow_zero_to_zero_is_one () =
    assert (
      FiniteField.eq
        (FiniteField.pow (FiniteField.zero ()) Z.zero)
        (FiniteField.one ()) )

  (** 0 ** n = 0, n != 0 *)
  let pow_zero_to_non_null_exponent_is_zero () =
    let n = Z.of_int (Random.int 1_000_000_000) in
    assert (
      FiniteField.eq
        (FiniteField.pow (FiniteField.zero ()) n)
        (FiniteField.zero ()) )

  let pow_zero_on_random_equals_one () =
    let r = FiniteField.random () in
    assert (FiniteField.eq (FiniteField.pow r Z.zero) (FiniteField.one ()))

  let pow_zero_on_one_equals_one () =
    let e = FiniteField.one () in
    assert (FiniteField.eq (FiniteField.pow e Z.zero) (FiniteField.one ()))

  let pow_one_on_random_element_equals_the_random_element () =
    let e = FiniteField.random () in
    assert (FiniteField.eq (FiniteField.pow e Z.one) e)

  let pow_two_on_random_element_equals_the_square () =
    let e = FiniteField.random () in
    assert (
      FiniteField.eq (FiniteField.pow e (Z.succ Z.one)) (FiniteField.square e)
    )

  (** x**(-n) = x**(g - 1 - n) where g is the order of the additive group *)
  let pow_to_negative_exponent () =
    let x = FiniteField.random () in
    let n = Z.of_int (Random.int 1_000_000_000) in
    assert (
      FiniteField.eq
        (FiniteField.pow x (Z.neg n))
        (FiniteField.pow x (Z.sub (Z.pred FiniteField.order) n)) )

  let pow_addition_property () =
    let g = FiniteField.random () in
    let x = Z.of_int (Random.int 1_000_000_000) in
    let y = Z.of_int (Random.int 1_000_000_000) in
    assert (
      FiniteField.eq
        (FiniteField.pow g (Z.add x y))
        (FiniteField.mul (FiniteField.pow g x) (FiniteField.pow g y)) )

  (** x**g = x where g = |(F, +, 0)| *)
  let pow_to_the_additive_group_order_equals_same_element () =
    let x = FiniteField.random () in
    assert (FiniteField.eq (FiniteField.pow x FiniteField.order) x)

  (** x**g = 1 where g = |(F, *, 1)| *)
  let pow_to_the_multiplicative_group_order_equals_one () =
    let x = FiniteField.random () in
    assert (
      FiniteField.eq
        (FiniteField.pow x (Z.pred FiniteField.order))
        (FiniteField.one ()) )

  (** x**(n + g) = x**n where g = |(F, *, 1)| *)
  let pow_add_multiplicative_group_order_to_a_random_power () =
    let x = FiniteField.random () in
    let n = Z.of_int (Random.int 1_000_000_000) in
    let order = Z.pred FiniteField.order in
    assert (
      FiniteField.eq (FiniteField.pow x (Z.add n order)) (FiniteField.pow x n)
    )

  let get_tests () =
    let open Alcotest in
    ( "Field properties",
      [ test_case "zero_nullifier_one" `Quick (repeat 1000 zero_nullifier_one);
        test_case
          "zero_nullifier_zero"
          `Quick
          (repeat 1000 zero_nullifier_zero);
        test_case
          "zero_nullifier_random"
          `Quick
          (repeat 1000 zero_nullifier_random);
        test_case
          "inverse_of_non_null_does_exist"
          `Quick
          (repeat 1000 inverse_of_non_null_does_exist);
        test_case
          "inverse_of_one_is_one"
          `Quick
          (repeat 1000 inverse_of_one_is_one);
        test_case
          "zero_has_no_inverse"
          `Quick
          (repeat 1000 zero_has_no_inverse);
        test_case "inverse_of_inverse" `Quick (repeat 1000 inverse_of_inverse);
        test_case
          "opposite_of_opposite"
          `Quick
          (repeat 1000 opposite_of_opposite);
        test_case
          "opposite_of_zero_is_zero"
          `Quick
          (repeat 1000 opposite_of_zero_is_zero);
        test_case
          "additive_associativity"
          `Quick
          (repeat 1000 additive_associativity);
        test_case "distributivity" `Quick (repeat 1000 distributivity);
        test_case
          "pow zero on random element equals one"
          `Quick
          (repeat 1000 pow_zero_on_random_equals_one);
        test_case
          "pow zero on one equals one"
          `Quick
          (repeat 1000 pow_zero_on_one_equals_one);
        test_case
          "pow one on random element equals the same element"
          `Quick
          (repeat 1000 pow_one_on_random_element_equals_the_random_element);
        test_case
          "pow two on random element equals the square"
          `Quick
          (repeat 1000 pow_one_on_random_element_equals_the_random_element);
        test_case
          "pow element to the additive group order"
          `Quick
          (repeat 1000 pow_to_the_additive_group_order_equals_same_element);
        test_case
          "pow element to the multiplicative group order"
          `Quick
          (repeat 1000 pow_to_the_multiplicative_group_order_equals_one);
        test_case
          "pow element to a random power plus the additive group order"
          `Quick
          (repeat 1000 pow_add_multiplicative_group_order_to_a_random_power);
        test_case
          "pow zero to zero is one"
          `Quick
          (repeat 1 pow_zero_to_zero_is_one);
        test_case
          "pow zero to non null exponent is zero"
          `Quick
          (repeat 1000 pow_zero_to_non_null_exponent_is_zero);
        test_case
          "pow to negative exponent"
          `Quick
          (repeat 1000 pow_to_negative_exponent);
        test_case
          "pow addition property"
          `Quick
          (repeat 1000 pow_addition_property);
        test_case
          "multiplicative_associativity"
          `Quick
          (repeat 1000 multiplicative_associativity) ] )
end
