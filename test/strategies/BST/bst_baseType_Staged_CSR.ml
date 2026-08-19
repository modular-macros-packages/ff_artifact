open Util.Limits
open Type;;
open Nat;;
open Fast_gen;;
open Core;;


  type t = tree [@@deriving sexp, quickcheck]

  macro staged_code : unit -> tree expr G_CSR.t = fun () ->
    G_CSR.recursive (G_CSR.C.lift ())
      (fun go ->
          fun _ ->
            let _pair__004_ = ((<< 1.  >>), (G_CSR.return (<< E  >>)))
            and _pair__005_ =
              ((<< 1.  >>),
                (G_CSR.bind (G_CSR.size ())
                  ~f:(fun _size__001_ ->
                        G_CSR.with_size ~size_c:(G_CSR.C.pred _size__001_)
                          (G_CSR.bind (G_CSR.recurse go (G_CSR.C.lift ()))
                              ~f:(fun _x__006_ ->
                                    G_CSR.bind
                                      (Nat.staged_quickcheck_generator_csr_t (G_CSR.C.lift 100))
                                      ~f:(fun _x__007_ ->
                                            G_CSR.bind
                                              (Nat.staged_quickcheck_generator_csr_t (G_CSR.C.lift 100))
                                              ~f:(fun _x__008_ ->
                                                    G_CSR.bind
                                                      (G_CSR.recurse go
                                                        (G_CSR.C.lift
                                                          ()))
                                                      ~f:(fun
                                                          _x__009_
                                                          ->
                                                          G_CSR.return
                                                          (<<
                                                          T
                                                          (($_x__009_),
                                                          ($_x__008_),
                                                          ($_x__007_),
                                                          ($_x__006_)) 
                                                          >>))))))))) in
            let _gen__002_ = G_CSR.weighted_union [_pair__004_]
            and _gen__003_ =
              G_CSR.weighted_union [_pair__004_; _pair__005_] in
            G_CSR.bind (G_CSR.size ())
              ~f:(fun x -> G_CSR.if_z x _gen__002_ _gen__003_))

    let quickcheck_generator = $(G_CSR.to_bq (staged_code ()))
  let make_quickcheck_generator () = quickcheck_generator

  let sexp_of_t = sexp_of_t