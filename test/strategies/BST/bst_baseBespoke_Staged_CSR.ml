open Util.Limits
open Type
open Nat;;
open G_CSR
open Let_syntax
type t = tree [@@deriving sexp, quickcheck]

macro staged_code : unit -> tree expr G_CSR.t = fun () ->
  bind (list (map2 (Nat.staged_quickcheck_generator_csr_t (G_CSR.C.lift 1000)) (Nat.staged_quickcheck_generator_csr_t (G_CSR.C.lift 1000)) ~f:(fun x y -> G_CSR.C.pair x y)))
  ~f:(fun l -> return << repeat_insert $l >>)

let quickcheck_generator = $(G_CSR.to_bq (staged_code ()))
let make_quickcheck_generator () = quickcheck_generator
