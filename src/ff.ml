(** General module signature for a finite field *)
module type T = sig
  type t

  val order : Z.t

  val of_bytes : Bytes.t -> t

  val to_bytes : t -> Bytes.t

  (** Create an empty value to store an element of the field. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN. USE IT AS A BUFFER *)
  val empty : unit -> t

  (* Let's use a function for the moment *)
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

module Make (S : sig
  val order : Z.t
end) : T = struct
  type t = Z.t

  let order = S.order

  let of_bytes b = Z.of_string (Bytes.to_string b)

  let to_bytes b = Bytes.of_string (Z.to_string b)

  (** Create an empty value to store an element of the field. DO NOT USE THIS TO
      DO COMPUTATIONS WITH, UNDEFINED BEHAVIORS MAY HAPPEN. USE IT AS A BUFFER *)
  let empty () = Z.zero

  (* Let's use a function for the moment *)
  let zero () = Z.zero

  let one () = Z.one

  let is_zero s = Z.equal (Z.rem s order) Z.zero

  let is_one s = Z.equal (Z.rem s order) Z.one

  let random () =
    let r = Random.int (Z.to_int S.order) in
    Z.of_int r

  let add a b = Z.rem (Z.add a b) order

  let mul a b = Z.rem (Z.mul a b) order

  let eq a b = Z.equal (Z.rem a order) (Z.rem b order)

  let negate a = Z.min order (Z.rem a order)

  (* Unsafe version of inverse *)
  let inverse a = Z.invert a order

  (* Safe version of inverse *)
  let inverse_opt a =
    try Some (Z.invert a order) with Division_by_zero -> None

  let square x = Z.mul x x

  let double x = Z.add x x

  let two_z = Z.succ Z.one

  let rec pow g n =
    if Z.equal n Z.zero then one ()
    else if Z.equal n Z.one then g
    else
      let (a, r) = Z.div_rem n two_z in
      let acc = pow g a in
      let acc_square = mul acc acc in
      if Z.equal r Z.zero then Z.rem acc_square order else mul acc_square g
end
