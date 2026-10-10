module G = Fast_gen.Staged_generator.MakeStaged$(Fast_gen.Sr_random)
open G
open Let_syntax

$let staged_generator : bool list expr G.t =
  recursive (G.C.lift ()) (fun r u ->
    let%bind n = size in
    let%bind leqz = split_bool << $n <= 0 >> in
    if leqz then return (<<[]>>)
    else
      let%bind x = bool in
      let%bind xs = with_size ~size_c:(<< $n - 1 >>) (recurse r u) in
      return (<< ($x :: $xs) >>)
  )

let quickcheck_generator = $(G.to_bq staged_generator)
let make_quickcheck_generator () = quickcheck_generator
