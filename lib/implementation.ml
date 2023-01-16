open! Core
include Implementation_intf

module Reference : S = struct
  type ('k, 'v, 'cmp) t =
    { comparator : ('k, 'cmp) Comparator.t
    ; mutable data : ('k, 'v, 'cmp) Map.t
    ; mutable compare_by : 'k * 'v -> 'k * 'v -> int
    ; mutable filter : 'k -> 'v -> bool
    }

  let comparison_of_comparator (comparator : _ Comparator.t) (k1, _) (k2, _) =
    comparator.compare k1 k2
  ;;

  let create (type k cmp) (comparator : (k, cmp) Comparator.t) =
    let data =
      Map.empty
        (module struct
          type t = k
          type comparator_witness = cmp

          let comparator = comparator
        end)
    in
    let compare_by = comparison_of_comparator comparator in
    let filter _ _ = true in
    { comparator; data; compare_by; filter }
  ;;

  (* mutators *)
  let add t key data = t.data <- Map.add_exn t.data ~key ~data
  let update t key ~old:_ data = t.data <- Map.set t.data ~key ~data
  let remove t key _data = t.data <- Map.remove t.data key

  (* sorting *)
  let set_sort t (c : _ Comparator.t) = t.compare_by <- c.compare
  let clear_sort t = t.compare_by <- comparison_of_comparator t.comparator

  (* filtering *)
  let set_filter t f = t.filter <- f
  let clear_filter t = t.filter <- (fun _ _ -> true)

  let get_range t ~low ~high =
    t.data
    |> Map.to_alist
    |> List.sort ~compare:t.compare_by
    |> List.filter ~f:(Tuple2.uncurry t.filter)
    |> Fn.flip List.drop low
    |> Fn.flip List.take (high - low)
  ;;
end

module Tester = struct
  module Op = struct
    type sort_kind =
      | Alpha
      | Alpha_caseless
      | Rev_alpha
      | Rev_alpha_caseless
      | All_zero
    [@@deriving quickcheck, sexp, equal, compare]

    type filter_kind =
      | On_length of int
      | All_true
      | All_false
    [@@deriving quickcheck, sexp, equal, compare]

    type t =
      | Add of
          { key : int
          ; data : string
          }
      | Update of
          { key : int
          ; old_data : string
          ; data : string
          }
      | Remove of
          { key : int
          ; data : string
          }
      | Set_sort of sort_kind
      | Clear_sort
      | Set_filter of filter_kind
      | Clear_filter
    [@@deriving quickcheck, sexp, equal, compare]

    let rec to_sort = function
      | Alpha -> fun (_, a) (_, b) -> String.compare a b
      | Alpha_caseless -> fun (_, a) (_, b) -> String.Caseless.compare a b
      | Rev_alpha -> Comparable.reverse (to_sort Alpha)
      | Rev_alpha_caseless -> Comparable.reverse (to_sort Alpha_caseless)
      | All_zero -> fun _ _ -> 0
    ;;

    let to_filter = function
      | All_true -> fun _ _ -> true
      | All_false -> fun _ _ -> false
      | On_length n -> fun _ s -> String.length s < n
    ;;
  end

  module Packet_group = struct
    type t = (Op.t list * int * int) list [@@deriving sexp, quickcheck, equal, compare]
  end

  module Make' (A : S) = struct
    let empty () = A.create Int.comparator
    let sexp_of_t = [%sexp_of: int * string]

    let apply t : Op.t -> unit = function
      | Add { key; data } -> A.add t key data
      | Update { key; old_data; data } -> A.update t key ~old:old_data data
      | Remove { key; data } -> A.remove t key data
      | Set_sort sort_kind ->
        let compare = Op.to_sort sort_kind in
        let module M = (val Comparator.(make ~compare ~sexp_of_t)) in
        A.set_sort t M.comparator
      | Clear_sort -> A.clear_sort t
      | Set_filter filter_kind ->
        let filter = Op.to_filter filter_kind in
        A.set_filter t filter
      | Clear_filter -> A.clear_filter t
    ;;
  end

  module Make (A : S) = struct
    module Ref = Make' (Reference)
    module Test = Make' (A)
    include Test

    let quickcheck_test () =
      Quickcheck.test Packet_group.quickcheck_generator ~f:(fun packet_group ->
        let ref = Ref.empty () in
        let test = Test.empty () in
        List.iter packet_group ~f:(fun (packet, low, high) ->
          List.iter packet ~f:(fun op ->
            Ref.apply ref op;
            Test.apply test op);
          let ref_out = Reference.get_range ref ~low ~high in
          let test_out = A.get_range test ~low ~high in
          assert ([%equal: (int * string) list] ref_out test_out)))
    ;;
  end
end
