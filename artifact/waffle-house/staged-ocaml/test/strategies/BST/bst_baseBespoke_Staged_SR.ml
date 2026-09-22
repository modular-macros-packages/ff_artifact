open Util.Limits
open Nat;;
open Type
type t = tree [@@deriving sexp, quickcheck]

$gen staged_code : unit -> tree expr G_SR.t = fun () ->
  G_SR.bind (G_SR.list (G_SR.map2 (Nat.staged_quickcheck_generator_sr_t (G_SR.C.lift 1000)) (Nat.staged_quickcheck_generator_sr_t (G_SR.C.lift 1000)) ~f:(fun x y -> G_SR.C.pair x y)))
  ~f:(fun l -> G_SR.return << repeat_insert $l >>)

let quickcheck_generator = $(G_SR.to_bq (staged_code ()))
let make_quickcheck_generator () = quickcheck_generator
