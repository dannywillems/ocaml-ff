(** Base module signature for a finite field *)
module type BASE = sig
  exception Not_in_field of Bytes.t

  type t

  (** The order of the finite field *)
  val order : Z.t

  (** minimal number of bytes required to encode a value of the field. *)
  val size_in_bytes : int

  (** [check_bytes bs] returns [true] if [bs] is a correct byte
      representation of a field element *)
  val check_bytes : Bytes.t -> bool

  (** The neutral element for the addition *)
  val zero : t

  (** The neutral element for the multiplication *)
  val one : t

  (** [is_zero x] returns [true] if [x] is the neutral element for the addition *)
  val is_zero : t -> bool

  (** [is_one x] returns [true] if [x] is the neutral element for the multiplication *)
  val is_one : t -> bool

  (** Use carefully!
      [random ()] returns a random element of the field. A state for the PRNG
      can be given to initialize the PRNG in the requested state. If no state is
      given, no initialisation is performed *)
  val random : ?state:Random.State.t -> unit -> t

  (** Use carefully!
      [non_null_random ()] returns a non null random element of the field.
      A state for the PRNG can be given to initialize the PRNG in the requested
      state. If no state is given, no initialisation is performed *)
  val non_null_random : ?state:Random.State.t -> unit -> t

  (** [add a b] returns [a + b mod order] *)
  val add : t -> t -> t

  (** Infix operator for [add] *)
  val ( + ) : t -> t -> t

  (** [mul a b] returns [a * b mod order] *)
  val mul : t -> t -> t

  (** Infix operator for [mul] *)
  val ( * ) : t -> t -> t

  (** [eq a b] returns [true] if [a = b mod order], else [false] *)
  val eq : t -> t -> bool

  (** Infix operator for [eq] *)
  val ( = ) : t -> t -> bool

  (** [negate x] returns [-x mod order]. Equivalently, [negate x] returns the
      unique [y] such that [x + y mod order = 0]
  *)
  val negate : t -> t

  (** Infix operator for [negate] *)
  val ( - ) : t -> t

  (** [inverse_exn x] returns [x^-1] if [x] is not [0], else raise
      [Division_by_zero]
  *)
  val inverse_exn : t -> t

  (** [inverse_opt x] returns [x^-1] if [x] is not [0] as an option, else [None] *)
  val inverse_opt : t -> t option

  (** [div_exn a b] returns [a * b^-1]. Raise [Division_by_zero] if [b = zero] *)
  val div_exn : t -> t -> t

  (** [div_opt a b] returns [a * b^-1] as an option. Return [None] if [b = zero] *)
  val div_opt : t -> t -> t option

  (** Infix operator for [div_exn] *)
  val ( / ) : t -> t -> t

  (** [square x] returns [x^2] *)
  val square : t -> t

  (** [double x] returns [2x] *)
  val double : t -> t

  (** [pow x n] returns [x^n] *)
  val pow : t -> Z.t -> t

  (** Infix operator for [pow] *)
  val ( ** ) : t -> Z.t -> t

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes of_bytes_exn t = t.
      Raise [Not_in_field] if the bytes do not represent an element in the field.
  *)
  val of_bytes_exn : Bytes.t -> t

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes (Option.get (of_bytes_opt t)) = t. By default, little endian encoding
      is used and the given element is modulo the prime order *)
  val of_bytes_opt : Bytes.t -> t option

  (** Convert the value t to a bytes representation which can be used for
      hashing for instance. It is not required that to_bytes of_bytes_exn t = t. By
      default, little endian encoding is used, and length of the resulting bytes
      may vary depending on the order.
  *)
  val to_bytes : t -> Bytes.t
end

(** Module type for prime field of the form GF(p) where p is prime *)
module type PRIME = sig
  include BASE

  (** Create a value t from a predefined string representation. It is not
      required that to_string of_string t = t. By default, decimal
      representation of the number is used, modulo the order of the field *)
  val of_string : string -> t

  (** String representation of a value t. It is not required that to_string
      of_string t = t. By default, decimal representation of the number is
      used *)
  val to_string : t -> string

  (** [of_z x] builds an element t from the Zarith element [x]. [mod order] is
      applied if [x >= order] *)
  val of_z : Z.t -> t

  (** [to_z x] builds a Zarith element, using the decimal representation.
      Arithmetic on the result can be done using the modular functions on
      integers *)
  val to_z : t -> Z.t
end

(** Module type for prime field with additional functions to manipulate roots of unity *)
module type PRIME_WITH_ROOT_OF_UNITY = sig
  include PRIME

  (** Returns a nth root of unity *)
  val get_nth_root_of_unity : Z.t -> t

  (** [is_nth_root_of_unity n x] returns [true] if [x] is a nth-root of unity*)
  val is_nth_root_of_unity : Z.t -> t -> bool
end

module MakeFp (S : sig
  val prime_order : Z.t
end) : PRIME_WITH_ROOT_OF_UNITY = struct
  exception Not_in_field of Bytes.t

  type t = Z.t

  let order =
    assert (S.prime_order >= Z.of_string "2") ;
    S.prime_order

  let log256 n = log n /. log 256.

  let size_in_bytes = int_of_float (log256 (Z.to_float order)) + 1

  let zero = Z.zero

  let one = Z.one

  (** By default, any bytes sequence is valid because we care about the result
      modulo the order *)
  let check_bytes _bs = true

  let is_zero s = Z.equal (Z.erem s order) Z.zero

  let is_one s = Z.equal (Z.erem s order) Z.one

  let random ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    let r = Bytes.init size_in_bytes (fun _ -> char_of_int (Random.int 256)) in
    Z.erem (Z.of_bits (Bytes.to_string r)) order

  let non_null_random ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    let rec aux () =
      let r = random () in
      if is_zero r then aux () else r
    in
    aux ()

  let add a b = Z.erem (Z.add a b) order

  let ( + ) = add

  let mul a b = Z.erem (Z.mul a b) order

  let ( * ) = mul

  let eq a b = Z.equal (Z.erem a order) (Z.erem b order)

  let ( = ) = eq

  let negate a = Z.sub order a

  let ( - ) = negate

  let inverse_exn a =
    if a = zero then raise Division_by_zero else Z.invert a order

  let inverse_opt a =
    try Some (Z.invert a order) with Division_by_zero -> None

  let div_exn a b =
    if b = zero then raise Division_by_zero else mul a (inverse_exn b)

  let div_opt a b = if b = zero then None else Some (mul a (inverse_exn b))

  let ( / ) = div_exn

  let square x = Z.mul x x

  let double x = Z.add x x

  let two_z = Z.succ Z.one

  let rec pow x n =
    if Z.equal n Z.zero then one
    else if is_zero x then zero
    else if Z.equal n Z.one then x
    else
      let n = Z.erem n (Z.pred order) in
      let (a, r) = Z.ediv_rem n two_z in
      let acc = pow x a in
      let acc_square = mul acc acc in
      if Z.equal r Z.zero then acc_square else mul acc_square x

  let ( ** ) = pow

  (* Decimal representation by default *)
  let of_string s = Z.erem (Z.of_string s) order

  (* Decimal representation by default *)
  let to_string s = Z.to_string s

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes (of_bytes_exn t)) = t. By default, little endian
      encoding is used and the given element is modulo the prime order *)
  let of_bytes_exn s = Z.erem (Z.of_bits (Bytes.to_string s)) order

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes (Option.get (of_bytes_opt t)) = t. By default,
      little endian encoding is used and the given element is modulo the prime order *)
  let of_bytes_opt s = Some (of_bytes_exn s)

  (* Little endian representation *)
  let to_bytes s =
    let b = Bytes.of_string (Z.to_bits s) in
    let res = Bytes.make size_in_bytes '\000' in
    Bytes.blit b 0 res 0 (min (Bytes.length b) size_in_bytes) ;
    res

  let rec get_nth_root_of_unity n =
    if not (Z.equal (Z.erem (Z.pred order) n) Z.zero) then
      failwith "n must divide the order of the multiplicate group"
    else
      let r = random () in
      if (not (eq r zero)) && eq (pow (pow r (Z.div (Z.pred order) n)) n) one
      then r
      else get_nth_root_of_unity n

  let is_nth_root_of_unity n x =
    if not (Z.equal (Z.erem (Z.pred order) n) Z.zero) then
      failwith "n must divide the order of the multiplicate group"
    else (not (eq x zero)) && eq (pow (pow x (Z.div (Z.pred order) n)) n) one

  let to_z t = t

  let of_z t = Z.erem t order
end

module MakeFp2
    (Fp : BASE) (Intf : sig
      (* Non square residue. Arithmetic is over Fp[X] / X^2 - r *)
      val nsr : Fp.t
    end) : sig
  include BASE

  val components : t -> Fp.t * Fp.t
end = struct
  (* The following arithmetic does not work with p = 2 *)

  ;;
  assert (Z.is_odd Fp.order)

  exception Not_in_field of Bytes.t

  type t = Fp.t * Fp.t

  let components (a, b) = (a, b)

  let order = Z.mul Fp.order Fp.order

  let size_in_bytes = Fp.size_in_bytes * 2

  let check_bytes b =
    if Bytes.length b = size_in_bytes then
      let x = Bytes.sub b 0 (size_in_bytes / 2) in
      let y = Bytes.sub b (size_in_bytes / 2) (size_in_bytes / 2) in
      Fp.check_bytes x && Fp.check_bytes y
    else false

  let zero = (Fp.zero, Fp.zero)

  let one = (Fp.one, Fp.zero)

  let is_zero (x, y) = Fp.(x = zero && y = zero)

  let is_one (x, y) = Fp.(x = Fp.one && y = Fp.zero)

  let random ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    (Fp.random (), Fp.random ())

  let non_null_random ?state () =
    (match state with None -> () | Some s -> Random.set_state s) ;
    let x = Random.bool () in
    if x then (Fp.non_null_random (), Fp.random ())
    else (Fp.random (), Fp.non_null_random ())

  let add (x1, y1) (x2, y2) = (Fp.(x1 + x2), Fp.(y1 + y2))

  let ( + ) = add

  let mul (x1, y1) (x2, y2) =
    let open Fp in
    let tmp_x = x1 * x2 in
    let tmp_y = y1 * y2 in
    let x' = tmp_x + (Intf.nsr * tmp_y) in
    let y' = (x1 * y2) + (y1 * x2) in
    (x', y')

  let ( * ) = mul

  let eq (x1, y1) (x2, y2) = Fp.(x1 = x2 && y1 = y2)

  let ( = ) = eq

  let negate (x, y) = (Fp.negate x, Fp.negate y)

  let ( - ) = negate

  let aux_inverse (x, y) =
    (* Let's use square in case of `*` is not optimised for the square case *)
    let x_square = Fp.square x in
    let y_square = Fp.square y in
    (* inverse of [x_square - nsr * y_square] *)
    let tmp_inverse =
      Fp.(inverse_exn (x_square + Fp.negate (Intf.nsr * y_square)))
    in
    let x' = Fp.(x * tmp_inverse) in
    let y' = Fp.(negate y * tmp_inverse) in
    (x', y')

  let inverse_exn x =
    if is_zero x then raise Division_by_zero else aux_inverse x

  let inverse_opt x = if is_zero x then None else Some (aux_inverse x)

  let div_exn a b =
    if b = zero then raise Division_by_zero else mul a (inverse_exn b)

  let div_opt a b = if b = zero then None else Some (mul a (inverse_exn b))

  let ( / ) = div_exn

  let square (a, b) =
    let ab = Fp.(a * b) in
    Fp.
      ( ((a + b) * (a + (Intf.nsr * b)))
        + Fp.negate ab
        + Fp.negate (Intf.nsr * ab),
        ab + ab )

  let double x = add x x

  let two_z = Z.succ Z.one

  let rec pow x n =
    if Z.equal n Z.zero then one
    else if is_zero x then zero
    else if Z.equal n Z.one then x
    else
      let n = Z.erem n (Z.pred order) in
      let (a, r) = Z.ediv_rem n two_z in
      let acc = pow x a in
      let acc_square = mul acc acc in
      if Z.equal r Z.zero then acc_square else mul acc_square x

  let ( ** ) = pow

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes (of_bytes_exn t)) = t. By default, little endian
      encoding is used and the given element is modulo the prime order *)
  let of_bytes_exn b =
    if Int.equal (Bytes.length b) size_in_bytes then
      let x_bytes = Bytes.sub b 0 (Int.div size_in_bytes 2) in
      let y_bytes =
        Bytes.sub b (Int.div size_in_bytes 2) (Int.div size_in_bytes 2)
      in
      (Fp.of_bytes_exn x_bytes, Fp.of_bytes_exn y_bytes)
    else raise (Not_in_field b)

  (** From a predefined bytes representation, construct a value t. It is not
      required that to_bytes (Option.get (of_bytes_opt t)) = t. By default,
      little endian encoding is used and the given element is modulo the prime order *)
  let of_bytes_opt b =
    if Int.equal (Bytes.length b) size_in_bytes then
      let x_bytes = Bytes.sub b 0 (Int.div size_in_bytes 2) in
      let y_bytes =
        Bytes.sub b (Int.div size_in_bytes 2) (Int.div size_in_bytes 2)
      in
      let x = Fp.of_bytes_opt x_bytes in
      let y = Fp.of_bytes_opt y_bytes in
      match (x, y) with
      | (None, _) | (_, None) -> None
      | (Some x, Some y) -> Some (x, y)
    else None

  (* Little endian representation *)
  let to_bytes (x, y) =
    let b = Bytes.make size_in_bytes '\000' in
    Bytes.blit (Fp.to_bytes x) 0 b 0 (Int.div size_in_bytes 2) ;
    Bytes.blit
      (Fp.to_bytes y)
      0
      b
      (Int.div size_in_bytes 2)
      (Int.div size_in_bytes 2) ;
    b
end
