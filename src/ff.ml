(** General module signature for a finite field *)
module type T = sig
  type t

  val order : Z.t

  (** minimal number of bytes required to encode a value of the field. *)
  val size_in_bytes : int

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

  (** Create a value t from a predefined string representation. It is not
      required that to_string of_string t = t. By default, decimal
      representation of the number is used, modulo the order of the field *)
  val of_string : string -> t

  (** String representation of a value t. It is not required that to_string
      of_string t = t. By default, decimal representation of the number is
      used *)
  val to_string : t -> string

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes of_bytes t = t. By default, little endian encoding
      is used and the given element is modulo the prime order *)
  val of_bytes : Bytes.t -> t

  (** Convert the value t to a bytes representation which can be used for
      hashing for instance. It is not required that to_bytes of_bytes t = t. By
      default, little endian encoding is used, and length of the resulting bytes
      may vary depending on the order.
  *)
  val to_bytes : t -> Bytes.t
end

module MakeFp (S : sig
  val prime_order : Z.t
end) : T = struct
  type t = Z.t

  let order =
    assert (S.prime_order >= Z.of_string "2") ;
    S.prime_order

  let log256 n = log n /. log 256.

  let size_in_bytes = int_of_float (log256 (Z.to_float order)) + 1

  (* Let's use a function for the moment *)
  let zero () = Z.zero

  let one () = Z.one

  let is_zero s = Z.equal (Z.erem s order) Z.zero

  let is_one s = Z.equal (Z.erem s order) Z.one

  let random () =
    Random.self_init ();
    let r = Random.int (Z.to_int order) in
    Z.of_int r

  let add a b = Z.erem (Z.add a b) order

  let mul a b = Z.erem (Z.mul a b) order

  let eq a b = Z.equal (Z.erem a order) (Z.erem b order)

  let negate a = Z.sub order a

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

  (* Decimal representation by default *)
  let of_string s = Z.erem (Z.of_string s) order

  (* Decimal representation by default *)
  let to_string s = Z.to_string s

  (* Bytes must be in little endian *)
  let of_bytes s = Z.erem (Z.of_bits (Bytes.to_string s)) order

  (* Little endian representation *)
  let to_bytes s = Bytes.of_string (Z.to_bits s)
end
