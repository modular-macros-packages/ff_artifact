open Fast_gen;;
open Core;;

module SizeRand : Base_quickcheck.Test.S with type t = Int.t * Int.t = struct
  type t = Int.t * Int.t [@@deriving quickcheck, sexp_of]

  open Base_quickcheck.Generator;;
  let quickcheck_generator =
    bind small_strictly_positive_int ~f:(fun x ->
      bind Base_quickcheck.Generator.int ~f:(fun y->
        return (x,y)
      )
    )
end

let failures = ref 0

let difftest ?config ~name:s exn eq g1 g2 =
  fun () ->
    match
      Base_quickcheck.Test.run_exn ?config
        ~f:(fun (size, seed) ->
          let v1 =
            Base_quickcheck.Generator.generate g1 ~size
              ~random:(Splittable_random.of_int seed)
          in
          let v2 =
            Base_quickcheck.Generator.generate g2 ~size
              ~random:(Splittable_random.of_int seed)
          in
          if eq v1 v2 then () else exn v1 v2)
        (module SizeRand)
    with
    | () -> Printf.printf "  ok   %s\n%!" s
    | exception e ->
        incr failures;
        Printf.printf "  FAIL %s: %s\n%!" s (Exn.to_string e)

exception Mismatch of string

let pair_test ?config ~name ~show ~equal g1 g2 =
  difftest ?config ~name
    (fun v1 v2 -> raise (Mismatch (show v1 ^ " <> " ^ show v2)))
    equal g1 g2

let run_suites suites =
  List.iter suites ~f:(fun (group, tests) ->
      Printf.printf "== %s ==\n%!" group;
      List.iter tests ~f:(fun t -> t ()));
  if !failures > 0 then (
    Printf.printf "%d FAILURES\n%!" !failures;
    Stdlib.exit 1)
  else Printf.printf "all ok\n%!"
