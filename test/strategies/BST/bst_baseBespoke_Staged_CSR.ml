open Util.Limits
open Type
open Nat;;
module G = Fast_gen.Staged_generator.MakeStaged$(Fast_gen.C_sr_dropin_random)
open G
open Let_syntax
type t = tree [@@deriving sexp, quickcheck]

$let staged_code : tree expr G.t =
  bind (list (map2 (Nat.staged_quickcheck_generator_csr_t (G.C.lift 1000)) (Nat.staged_quickcheck_generator_csr_t (G.C.lift 1000)) ~f:(fun x y -> G.C.pair x y)))
  ~f:(fun l -> return << repeat_insert $l >>)

let quickcheck_generator = $(G.to_bq staged_code)
let make_quickcheck_generator () = quickcheck_generator
