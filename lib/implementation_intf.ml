open! Core

module type S = sig
  type ('k, 'v, 'cmp) t

  val create : ('k, 'cmp) Comparator.t -> ('k, _, 'cmp) t

  (* functions for setting the contents of the structure *)
  val add : ('k, 'v, _) t -> 'k -> 'v -> unit
  val update : ('k, 'v, _) t -> 'k -> old:'v -> 'v -> unit
  val remove : ('k, 'v, _) t -> 'k -> 'v -> unit

  (* sorting modifiers *)
  val set_sort : ('k, 'v, 'cmp) t -> ('k * 'v, _) Comparator.t -> unit
  val clear_sort : _ t -> unit

  (* filter modifiers *)
  val set_filter : ('k, 'v, _) t -> ('k -> 'v -> bool) -> unit
  val clear_filter : _ t -> unit

  (* retrieve *)
  val get_range : ('k, 'v, _) t -> low:int -> high:int -> ('k * 'v) list
end

module type Implementation = sig
  module type S = S

  module Reference : S

  module Tester : sig
    module Op : sig
      type t [@@deriving quickcheck, sexp, equal, compare]
    end

    module Make (A : S) : sig
      val empty : unit -> (int, string, Int.comparator_witness) A.t
      val apply : (int, string, _) A.t -> Op.t -> unit
      val quickcheck_test : unit -> unit
    end
  end
end
