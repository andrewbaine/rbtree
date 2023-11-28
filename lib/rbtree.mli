
module type Elt = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type t
  type elt
  val empty: t
  val member : elt -> t -> bool
  val insert : elt -> t -> t
  val delete : elt -> t -> t
  val of_list : elt List.t -> t
  val to_list : t -> elt List.t
end

module Make (Elt : Elt) : S with type elt := Elt.t

module Make_checked (Elt : Elt) : S with type elt := Elt.t
