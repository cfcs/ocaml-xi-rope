(** Implementation of the CRDT data structure used by xi-editor *)

module type CRDT_element = sig
  type t
  (** [t] is a (potentially) abstract type representing the element leaves
      in the CRDT data structure.*)

  val equal : t -> t -> bool
  (** [equal a b] is [true] if [a] and [b] are structurally equivalent.*)

  val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
  (** [pp fmt t] is [t] pretty-printed on [fmt].*)
end

module CRDT(E: CRDT_element) :
sig

    module rec Marker : sig
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

      val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]

      val equal : t -> t -> bool
      (** We expose equality, but since lt/gt comparisons are expensive
          due to the topological ordering, we hide those.*)

      val compare : t -> t -> int
      (* TODO didn't hide it *)

      val of_int64 : int64 -> t
      (** [of_int64 num] is a marker represented by [num].
          [num] should be random, and is not validated against
          existing [t]'s in the set.
          You should generally use {!generate} instead, this function is here
          to enable deserialization.*)

      val to_int64 : t -> int64
      (** [to_int64 t] is the inverse of {of_int64}.
          NB: Since markers are sorted topologically according to the edges
              in the graph, comparisons on [to_int64 t] are generally
              meaningless. This function is here to enable serialization only.
      *)

    end

  type elt = E.t
  (** The type of the underlying elements, ie the user's data.*)


  type element =
    | Live of elt
    | Tombstone of elt

  type edge =
    { left: Marker.t ;
      right: Marker.t ;
    }

  module Edges : sig
    type t
    val to_seq : t -> edge Seq.t
    val of_seq : edge Seq.t -> t
  end

  module Markers : sig
    include module type of Map.Make(Marker)
    val diff : 'a t -> 'a t -> 'a t
    val pp : Format.formatter -> element t -> unit
  end

  type author_id = int32
  (** The type used to represent an author. Used for "tie" resolution.*)

  type edit =
    { undo_group_id : unit ; (* TODO *)
      deleted : element Markers.t ;
      inserted : element Markers.t ;
      edges : Edges.t;
      author : author_id;
    }

  module Edits: sig
    type t
    val to_seq : t -> edit Seq.t
    val of_seq : edit Seq.t -> t
  end

  type t =
    { elements : element Markers.t ;
      edges    : Edges.t ;
      edits    : Edits.t ;
      authors  : author_id Markers.t ;
    }
    (** A CRDT state*)

  type diff = t
  (** The difference between two {t}'s. See {diff}.*)

    val pp : Format.formatter -> t -> unit [@@ocaml.toplevel_printer]
    (** [pp fmt t] is [t] pretty-printed on [fmt].*)

    val equal : t -> t -> bool
    (** [equal a b] is [true] if [a] and [b] containg exactly the same elements,
        edges and edits.*)

    val pp_elt : Format.formatter -> E.t -> unit [@@ocaml.toplevel_printer]
    (** [pp_elt] is {!E.pp}.*)

    val pp_element : Format.formatter -> element -> unit
    [@@ocaml.toplevel_printer]

    val element_equal : element -> element -> bool
    (** [element_equal a b] is [{!E.equal} a b] if
        [a] and [b] are both {!element.Live} or {!element.Tombstone};
        otherwise [false].*)

    val element_equal_ignoring_status : element -> element -> bool
    (** [element_equal_ignoring_status a b] is [{!E.equal} a b], regardless of
        the liveness of the two. See {!element_equal}.*)

    val empty : t
    (** [empty] is a [t] without any elements, edges or edits. *)

    val singleton : author_id -> Marker.t -> E.t -> t
    (** [singleton author marker element] is a new {!t} consisting of a
        single element [element] denoted by [marker],
        and authored by [author].*)

    val merge : t -> t -> t
    (** [merge t1 t2] is the applied union of [t1] and [t2].*)

    val merge_diff : t -> diff -> t

    val diff : t -> t -> diff
    (** [diff t1 t2] is [t3] for which [merge_diff t1 t3 = t2].
        Note that the inverse is not true, ie [merge t2 t3 <> t1].
        TODO Note that a diff contains only the minimum required updates,
             and as thus ie obtaining a {Snapshot.t} is not possible.
    *)

    val insert_element : t -> author:author_id ->
      after:Marker.t -> before:Marker.t -> Marker.t -> E.t -> t
    (** [append_element t author insertpoint new_marker new_element] is
        [{!merge} t ({!singleton} author new_marker new_element)] with the
        singleton being inserted into [t] immediately after [insertpoint].
    *)

    val kill_marker : t -> Marker.t -> t
    (** [kill_marker t marker] is [t] with the {!element} corresponding to
        [marker] marked as a {!element.Tombstone}.*)

    module Snapshot : sig
      type snapshot = ((author_id * Marker.t) * element) Pvec.t
      (* TODO make private *)

      val live_elements : snapshot -> (Marker.t * elt) Pvec.t
      (** [live_elements snapshot] is a new {!Pvec.t} vector
          consisting of the subset of {!element.Live} {!elt}'s in [snapshot].*)

      val pp : Format.formatter -> snapshot -> unit
      [@@ocaml.toplevel_printer]

      val equal : snapshot -> snapshot -> bool
      (** [equal a b] is [true] if [a] and [b] are equivalent,
          that is, if they have identical length;
          and each corresponding element is equivalent according to
          {!Marker.equal} and {!E.equal}, and have the
          same {!element.Live}/{!element.Tombstone} status.*)

      val to_vector : snapshot -> (author_id * E.t) Pvec.t
      (** [to_vector snap] is a {!Pvec.t} with [snap]'s {b live} elements.*)

      val extend_with_vector : author:author_id -> snapshot -> E.t Pvec.t ->
        snapshot
      (** [extend_with_vector ~author snap vector] is [snap] merged with
          [vector],
          with [vector]'s elements all being marked as {!element.Live} and
          as authored by [author],
          and with elements from [snap] that {b are not} present in [vector]
          marked as {!element.Tombstone}'d.
          Where possible, the markers from [snap] are reused to produce the
          smallest valid delta; otherwise {!Marker.generate} is used to create
          new markers, and the new elements appended {b to the right}.*)

      val of_t : t -> snapshot
      (** [of_t t] is the computed and ordered elements of the CRDT state [t]
          represented as a vector containing all live and tombstoned elements.*)
    end

    val update_with_vector : author_id -> t -> E.t Pvec.t -> t
    (** [update_with_vector author state vector] is [state] updated with
        [vector], marking elements present in [vector] as {!element.Live}, and
        elements only present in [state] as {!element.Tombstone}'d.
        New elements are assigned to the [author] id.
        See {!Snapshot.update_with_vector}, which is used internally.
    *)

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
