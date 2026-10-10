open Util.Limits
open Nat;;
module G = Fast_gen.Staged_generator.MakeStaged$(Fast_gen.Sr_random)
open Type
type t = tree [@@deriving sexp, quickcheck]

$let staged_code : tree expr G.t =
  G.bind (G.list (G.map2 (Nat.staged_quickcheck_generator_sr_t (G.C.lift 1000)) (Nat.staged_quickcheck_generator_sr_t (G.C.lift 1000)) ~f:(fun x y -> G.C.pair x y)))
  ~f:(fun l -> G.return << repeat_insert $l >>)

let quickcheck_generator = $(G.to_bq staged_code)
let make_quickcheck_generator () = quickcheck_generator
