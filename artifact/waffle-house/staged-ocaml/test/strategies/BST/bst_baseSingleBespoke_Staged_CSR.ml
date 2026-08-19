open Util.Limits;;
open Base;;
open Type;;
open Fast_gen;;
open Nat;;
open G_CSR
open Let_syntax

type t = tree [@@deriving sexp, quickcheck]

macro staged_quickcheck_generator : int expr -> int expr -> tree expr G_CSR.t = fun lo hi ->
  recursive (<< ($lo, $hi) >>)
  (fun go lohi ->
    let%bind sz = size () in
    let%bind (lo, hi) = split_pair lohi in
    let%bind should_stop = split_bool << $hi <= $lo || $sz <= 1 >> in
    if should_stop
      then return << E >>
    else
      weighted_union [
        (<< 1. >>, return << E >>);
        ((G_CSR.C.i2f sz), (
          let%bind k = int_inclusive ~lo ~hi in
          let%bind v = (Nat.staged_quickcheck_generator_csr_t (G_CSR.C.lift 1000)) in
          let%bind left = with_size ~size_c:(G_CSR.C.div2 sz) (recurse go << ($lo, $k - 1) >>) in
          let%bind right = with_size ~size_c:(G_CSR.C.div2 sz) (recurse go << ($k + 1, $hi) >>) in
          return (<< T ($left, $k, $v, $right) >>)))
      ]
  )

macro staged_code : unit -> tree expr G_CSR.t = fun () ->
  staged_quickcheck_generator (G_CSR.C.lift 0) (G_CSR.C.lift 1000)

let quickcheck_generator = $(G_CSR.to_bq (staged_code ()))
let make_quickcheck_generator () = quickcheck_generator

let compile_quickcheck_generator () = make_quickcheck_generator ()
