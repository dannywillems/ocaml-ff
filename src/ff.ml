(** General module signature for a finite field *)
module type T = sig
  type t

  val order : Z.t

  val zero : unit -> t

  val one : unit -> t

  val is_zero : t -> bool

  val is_one : t -> bool

  val random : unit -> t

  val add : t -> t -> t

  val mul : t -> t -> t

  val eq : t -> t -> bool

  val negate : t -> t

  (* Unsafe version of inverse *)
  val inverse : t -> t

  (* Safe version of inverse *)
  val inverse_opt : t -> t option

  val square : t -> t

  val double : t -> t

  val pow : t -> Z.t -> t
end

module MakeFp (S : sig
  val prime_order : Z.t
end) : T = struct
  type t = Z.t

  let order =
    assert (S.prime_order >= Z.of_string "2") ;
    S.prime_order

  (* Let's use a function for the moment *)
  let zero () = Z.zero

  let one () = Z.one

  let is_zero s = Z.equal (Z.erem s order) Z.zero

  let is_one s = Z.equal (Z.erem s order) Z.one

  let random () =
    let r = Random.int (Z.to_int order) in
    Z.of_int r

  let add a b = Z.erem (Z.add a b) order

  let mul a b = Z.erem (Z.mul a b) order

  let eq a b = Z.equal (Z.erem a order) (Z.erem b order)

  let negate a = Z.min order (Z.rem a order)

  (* Unsafe version of inverse *)
  let inverse a = Z.invert a order

  (* Safe version of inverse *)
  let inverse_opt a =
    try Some (Z.invert a order) with Division_by_zero -> None

  let square x = Z.mul x x

  let double x = Z.add x x

  let two_z = Z.succ Z.one

  let rec pow x n =
    if Z.equal n Z.zero then one ()
    else if is_zero x then zero ()
    else if Z.equal n Z.one then x
    else
      let n = Z.erem n (Z.pred order) in
      let (a, r) = Z.ediv_rem n two_z in
      let acc = pow x a in
      let acc_square = mul acc acc in
      if Z.equal r Z.zero then acc_square else mul acc_square x
end
