type 'a code = 'a expr
open Type;;
open Fast_gen;;
open Core;;
open Core_unix;;

open Gens

type t = expr [@@deriving quickcheck, sexp]
$gen staged_quickcheck_generator_typ : unit -> typ code G_SR.t = fun () ->
  G_SR.recursive (G_SR.C.lift ())
    (fun go ->
       fun _ ->
         let _pair__004_ =
           ((<< 1.  >>), (G_SR.return (<< TBool  >>)))
         and _pair__005_ =
           ((<< 1.  >>),
             (G_SR.bind (G_SR.size ())
                ~f:(fun _size__001_ ->
                      G_SR.with_size ~size_c:(G_SR.C.pred _size__001_)
                        (G_SR.bind (G_SR.recurse go (G_SR.C.lift ()))
                           ~f:(fun _x__006_ ->
                                 G_SR.bind
                                   (G_SR.recurse go (G_SR.C.lift ()))
                                   ~f:(fun _x__007_ ->
                                         G_SR.return
                                           (<<
                                              TFun
                                                (($_x__007_),
                                                  ($_x__006_)) 
                                              >>))))))) in
         let _gen__002_ = G_SR.weighted_union [_pair__004_]
         and _gen__003_ =
           G_SR.weighted_union [_pair__004_; _pair__005_] in
         G_SR.bind (G_SR.size ())
           ~f:(fun x -> G_SR.if_z x _gen__002_ _gen__003_))
$gen staged_code : unit -> t code G_SR.t = fun () ->
    G_SR.recursive (G_SR.C.lift ())
      (fun go ->
         fun _ ->
           let _pair__042_ =
             ((<< 1.  >>),
               (G_SR.bind (G_SR.int ())
                  ~f:(fun _x__050_ ->
                        G_SR.return (<< Var ($_x__050_)  >>))))
           and _pair__043_ =
             ((<< 1.  >>),
               (G_SR.bind (G_SR.bool ())
                  ~f:(fun _x__051_ ->
                        G_SR.return (<< Bool ($_x__051_)  >>))))
           and _pair__044_ =
             ((<< 1.  >>),
               (G_SR.bind (G_SR.size ())
                  ~f:(fun _size__039_ ->
                        G_SR.with_size ~size_c:(G_SR.C.pred _size__039_)
                          (G_SR.bind (G_SR.recurse go (G_SR.C.lift ()))
                             ~f:(fun _x__046_ ->
                                   G_SR.bind
                                     (staged_quickcheck_generator_typ ())
                                     ~f:(fun _x__047_ ->
                                           G_SR.return
                                             (<<
                                                Abs
                                                  (($_x__047_),
                                                    ($_x__046_)) 
                                                >>)))))))
           and _pair__045_ =
             ((<< 1.  >>),
               (G_SR.bind (G_SR.size ())
                  ~f:(fun _size__039_ ->
                        G_SR.with_size ~size_c:(G_SR.C.pred _size__039_)
                          (G_SR.bind (G_SR.recurse go (G_SR.C.lift ()))
                             ~f:(fun _x__048_ ->
                                   G_SR.bind
                                     (G_SR.recurse go (G_SR.C.lift ()))
                                     ~f:(fun _x__049_ ->
                                           G_SR.return
                                             (<<
                                                App
                                                  (($_x__049_),
                                                    ($_x__048_)) 
                                                >>))))))) in
           let _gen__040_ =
             G_SR.weighted_union [_pair__042_; _pair__043_]
           and _gen__041_ =
             G_SR.weighted_union
               [_pair__042_; _pair__043_; _pair__044_; _pair__045_] in
           G_SR.bind (G_SR.size ())
             ~f:(fun x -> G_SR.if_z x _gen__040_ _gen__041_))
let quickcheck_generator = $(G_SR.to_bq (staged_code ()))
let make_quickcheck_generator () = quickcheck_generator
let sexp_of_t = sexp_of_t