open Util.Limits
open Type;;
open Nat;;


  type t = tree [@@deriving sexp, quickcheck]
  macro staged_code : unit -> tree expr G_SR.t = fun () ->
    G_SR.recursive (G_SR.C.lift ())
      (fun go ->
          fun _ ->
            let _pair__004_ = ((<< 1.  >>), (G_SR.return (<< E  >>)))
            and _pair__005_ =
              ((<< 1.  >>),
                (G_SR.bind (G_SR.size ())
                  ~f:(fun _size__001_ ->
                        G_SR.with_size ~size_c:(G_SR.C.pred _size__001_)
                          (G_SR.bind (G_SR.recurse go (G_SR.C.lift ()))
                              ~f:(fun _x__006_ ->
                                    G_SR.bind
                                      (Nat.staged_quickcheck_generator_sr_t (G_SR.C.lift 100))
                                      ~f:(fun _x__007_ ->
                                            G_SR.bind
                                              (Nat.staged_quickcheck_generator_sr_t (G_SR.C.lift 100))
                                              ~f:(fun _x__008_ ->
                                                    G_SR.bind
                                                      (G_SR.recurse go
                                                        (G_SR.C.lift
                                                          ()))
                                                      ~f:(fun
                                                          _x__009_
                                                          ->
                                                          G_SR.return
                                                          (<<
                                                          T
                                                          (($_x__009_),
                                                          ($_x__008_),
                                                          ($_x__007_),
                                                          ($_x__006_)) 
                                                          >>))))))))) in
            let _gen__002_ = G_SR.weighted_union [_pair__004_]
            and _gen__003_ =
              G_SR.weighted_union [_pair__004_; _pair__005_] in
            G_SR.bind (G_SR.size ())
              ~f:(fun x -> G_SR.if_z x _gen__002_ _gen__003_))

    let quickcheck_generator = $(G_SR.to_bq (staged_code ()))
  let make_quickcheck_generator () = quickcheck_generator

  let sexp_of_t = sexp_of_t