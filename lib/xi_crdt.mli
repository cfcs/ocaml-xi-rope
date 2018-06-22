(** Implementation of the CRDT data structure used by xi-editor *)

module type CRDT_element = sig
  type t
  val equal : t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

module CRDT(E: CRDT_element) :
  sig

    type t
    (** A CRDT state*)

    val pp : Format.formatter -> t -> unit
    (** [pp fmt t] is [t] pretty-printed on [fmt].*)

    type author_id = int32
    (** The type used to represent an author. Used for "tie" resolution.*)

    type elt = E.t
    (** The type of the underlying elements, ie the user's data.*)

    type element = private | Live of elt
                           | Tombstone of elt
    val pp_element : Format.formatter -> element -> unit

    module Marker : sig
      type t (** A marker uniquely represents an element in the set *)

      val beginning : t (** The beginning of the rope *)

      val ending    : t
      (** Magic value signifying the ending of the rope.
          In this implementation [ending] is also the upper bound for [t]'s,
          allowing the implementation to choose a storage type capable of
          representing [t]'s between [beginning] and [ending], inclusively.*)

      val generate : unit -> t
      (** [generate ()] is a new [t] guaranteed to have an internal
          representation lying between [beginning] and [ending], exclusively.
          See {!ending}.*)

      val pp : Format.formatter -> t -> unit
      val equal : t -> t -> bool
      (** We expose equality, but since lt/gt comparisons are expensive,
          we hide those.*)

      val of_int64 : int64 -> t
      (** [of_int64 num] is a marker represented by [num].
          [num] should be random, and is not validated against
          existing [t]'s in the set.
          You should generally use {!generate} instead.*)

    end

    val singleton : author_id -> Marker.t -> E.t -> t
    (** [singleton author marker element] is a new {!t} consisting of a
        single element [element] denoted by [marker],
        and authored by [author].*)

    val merge : t -> t -> t
    (** [merge t1 t2] is the applied union of [t1] and [t2].*)

    val append : t -> author:author_id -> after:Marker.t -> Marker.t -> E.t -> t
    (** [append t author insertpoint new_marker new_element] is
        [{!merge} t ({!singleton} author new_marker new_element)] with the
        singleton being inserted into [t] immediately after [insertpoint].
    *)

    module Snapshot : sig
      type snapshot = private (Marker.t * element) Pvec.t

      val pp : Format.formatter -> snapshot -> unit

      val equal : snapshot -> snapshot -> bool
      (** [equal a b] is [true] if [a] and [b] are equivalent,
          that is, if they have identical length;
          and each corresponding element is equivalent according to
          {!Marker.equal} and {!E.equal}, and have the
          same {!Alive}/{!Tombstone} status.*)

      val to_vector : snapshot -> E.t Pvec.t

      val of_t : t -> snapshot
      (** [of_t t] is the computed and ordered elements of the CRDT state [t]
          represented as a vector containing all live and tombstoned elements.*)
    end

  end

(** An applied instance of the {!CRDT} functor with {!char} elements *)
module CharCRDT : sig
  module Char : sig
    type t = char
    include CRDT_element with type t := char
  end
  include module type of CRDT(Char)
end

(** An applied instance of the {!CRDT} functor with {!Uchar.t} elements *)
module UcharCRDT : sig
  module Uchar : sig
    include module type of Uchar with type t = Uchar.t
    include CRDT_element with type t := Uchar.t
  end
  include module type of CRDT(Uchar)
end
