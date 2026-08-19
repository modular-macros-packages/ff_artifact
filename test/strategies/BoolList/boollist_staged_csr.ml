open Gens
open G_CSR
open Let_syntax

macro staged_generator : unit -> bool list expr G_CSR.t = fun () ->
    recursive (G_CSR.C.lift ()) (fun r u ->
      let%bind n = size () in
      let%bind leqz = split_bool << $n <= 0 >> in
      if leqz then return (<<[]>>)
      else
        let%bind x = bool () in
        let%bind xs = with_size ~size_c:(<< $n - 1 >>) (recurse r u) in
        return (<< ($x :: $xs) >>)
    )

let quickcheck_generator = $(G_CSR.to_bq (staged_generator ()))
let make_quickcheck_generator () = quickcheck_generator
