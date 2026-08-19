
open Fast_gen
open Difftest

let qc_cfg =
  { Base_quickcheck.Test.default_config with
    test_count = 1000
  }


module Cases [R : Fast_gen.Random_intf.S] = struct
  module G = Fast_gen.Staged_generator.MakeStaged[R]

  macro int_g : unit -> int expr G.t = fun () -> G.int ()
  macro int_uniform_g : unit -> int expr G.t = fun () -> G.int_uniform ()

  macro int_uniform_inclusive_g : unit -> int expr G.t = fun () ->
    G.int_uniform_inclusive ~lo:(G.C.lift (-10000)) ~hi:(G.C.lift 10000)

  macro int_inclusive_g : unit -> int expr G.t = fun () ->
    G.int_inclusive ~lo:(G.C.lift 0) ~hi:(G.C.lift 100)

  macro int_log_uniform_inclusive_g : unit -> int expr G.t = fun () ->
    G.int_log_uniform_inclusive ~lo:(G.C.lift 0) ~hi:(G.C.lift 100)

  macro int_log_inclusive_g : unit -> int expr G.t = fun () ->
    G.int_log_inclusive ~lo:(G.C.lift 0) ~hi:(G.C.lift 100)

  macro float_ex_g : unit -> float expr G.t = fun () ->
    G.float_uniform_exclusive ~lo:(G.C.lift 0.0) ~hi:(G.C.lift 1.0)

  macro float_in_g : unit -> float expr G.t = fun () ->
    G.float_uniform_inclusive ~lo:(G.C.lift 0.0) ~hi:(G.C.lift 1.0)

  macro bool_g : unit -> bool expr G.t = fun () -> G.bool ()

  macro bind_order_g : unit -> (int * int) expr G.t = fun () ->
    G.bind (G.int ()) ~f:(fun nc ->
        G.bind (G.int ()) ~f:(fun nc' -> G.return (G.C.pair nc' nc)))

  macro choose_g : unit -> int expr G.t = fun () ->
    G.weighted_union
      [ (G.C.lift 1., G.return (G.C.lift 500));
        (G.C.lift 2., G.return (G.C.lift 1000));
        (G.C.lift 1., G.return (G.C.lift 100)) ]

  macro choose_bind1_g : unit -> int expr G.t = fun () ->
    G.bind (G.size ()) ~f:(fun nc ->
        G.weighted_union
          [ (G.C.lift 1., G.return (G.C.lift 22));
            (G.C.i2f nc, G.return (G.C.lift 1000));
            (G.C.lift 1., G.return (G.C.lift 100)) ])

  macro choose_bind2_g : unit -> (int * int) expr G.t = fun () ->
    G.bind (G.int ()) ~f:(fun i ->
        G.weighted_union
          [ ( G.C.lift 55.,
              G.bind (G.int ()) ~f:(fun j -> G.return (G.C.pair j i)) );
            (G.C.lift 1., G.return (G.C.pair (G.C.lift 1) (G.C.lift 2))) ])

  macro const_false_list_g : unit -> bool list expr G.t = fun () ->
    G.list (G.return (G.C.lift false))

  macro size_list_g : unit -> int list expr G.t = fun () -> G.list (G.size ())
  macro bool_list_g : unit -> bool list expr G.t = fun () -> G.list (G.bool ())

  macro bool_list_list_g : unit -> bool list list expr G.t = fun () ->
    G.list (G.list (G.bool ()))
end

module CSR = Cases[Fast_gen.Sr_random]
module CC = Cases[Fast_gen.C_random]
module CCSR = Cases[Fast_gen.C_sr_dropin_random]


module B = Bq_generator

let bq_int = B.int
let bq_int_uniform = B.int_uniform
let bq_int_uniform_inclusive = B.int_uniform_inclusive ~lo:(-10000) ~hi:10000
let bq_int_inclusive = B.int_inclusive ~lo:0 ~hi:100
let bq_int_log_uniform_inclusive = B.int_log_uniform_inclusive ~lo:0 ~hi:100
let bq_int_log_inclusive = B.int_log_inclusive ~lo:0 ~hi:100
let bq_float_ex = B.float_uniform_exclusive ~lo:0.0 ~hi:1.0
let bq_float_in = B.float_uniform_inclusive ~lo:0.0 ~hi:1.0
let bq_bool = B.bool

let bq_bind_order =
  B.bind B.int ~f:(fun n ->
      B.bind B.int ~f:(fun n' -> B.return (n', n)))

let bq_choose =
  B.weighted_union [ (1., B.return 500); (2., B.return 1000); (1., B.return 100) ]

let bq_choose_bind1 =
  B.bind B.size ~f:(fun n ->
      B.weighted_union
        [ (1., B.return 22);
          (Float.of_int n, B.return 1000);
          (1., B.return 100) ])

let bq_choose_bind2 =
  B.bind B.int ~f:(fun i ->
      B.weighted_union
        [ (55., B.bind B.int ~f:(fun j -> B.return (j, i)));
          (1., B.return (1, 2)) ])

let bq_const_false_list = B.list (B.return false)
let bq_size_list = B.list B.size
let bq_bool_list = B.list B.bool
let bq_bool_list_list = B.list (B.list B.bool)


let sr_int = $(CSR.G.to_bq (CSR.int_g ()))
let sr_int_uniform = $(CSR.G.to_bq (CSR.int_uniform_g ()))
let sr_int_uniform_inclusive = $(CSR.G.to_bq (CSR.int_uniform_inclusive_g ()))
let sr_int_inclusive = $(CSR.G.to_bq (CSR.int_inclusive_g ()))
let sr_int_log_uniform_inclusive = $(CSR.G.to_bq (CSR.int_log_uniform_inclusive_g ()))
let sr_int_log_inclusive = $(CSR.G.to_bq (CSR.int_log_inclusive_g ()))
let sr_float_ex = $(CSR.G.to_bq (CSR.float_ex_g ()))
let sr_float_in = $(CSR.G.to_bq (CSR.float_in_g ()))
let sr_bool = $(CSR.G.to_bq (CSR.bool_g ()))
let sr_bind_order = $(CSR.G.to_bq (CSR.bind_order_g ()))
let sr_choose = $(CSR.G.to_bq (CSR.choose_g ()))
let sr_choose_bind1 = $(CSR.G.to_bq (CSR.choose_bind1_g ()))
let sr_choose_bind2 = $(CSR.G.to_bq (CSR.choose_bind2_g ()))
let sr_const_false_list = $(CSR.G.to_bq (CSR.const_false_list_g ()))
let sr_size_list = $(CSR.G.to_bq (CSR.size_list_g ()))
let sr_bool_list = $(CSR.G.to_bq (CSR.bool_list_g ()))
let sr_bool_list_list = $(CSR.G.to_bq (CSR.bool_list_list_g ()))

let c_int = $(CC.G.to_bq (CC.int_g ()))
let c_int_uniform = $(CC.G.to_bq (CC.int_uniform_g ()))
let c_int_uniform_inclusive = $(CC.G.to_bq (CC.int_uniform_inclusive_g ()))
let c_int_inclusive = $(CC.G.to_bq (CC.int_inclusive_g ()))
let c_int_log_uniform_inclusive = $(CC.G.to_bq (CC.int_log_uniform_inclusive_g ()))
let c_int_log_inclusive = $(CC.G.to_bq (CC.int_log_inclusive_g ()))
let c_float_ex = $(CC.G.to_bq (CC.float_ex_g ()))
let c_float_in = $(CC.G.to_bq (CC.float_in_g ()))
let c_bool = $(CC.G.to_bq (CC.bool_g ()))

let csr_int = $(CCSR.G.to_bq (CCSR.int_g ()))
let csr_int_uniform = $(CCSR.G.to_bq (CCSR.int_uniform_g ()))
let csr_int_uniform_inclusive = $(CCSR.G.to_bq (CCSR.int_uniform_inclusive_g ()))
let csr_int_inclusive = $(CCSR.G.to_bq (CCSR.int_inclusive_g ()))
let csr_int_log_uniform_inclusive = $(CCSR.G.to_bq (CCSR.int_log_uniform_inclusive_g ()))
let csr_int_log_inclusive = $(CCSR.G.to_bq (CCSR.int_log_inclusive_g ()))
let csr_float_ex = $(CCSR.G.to_bq (CCSR.float_ex_g ()))
let csr_float_in = $(CCSR.G.to_bq (CCSR.float_in_g ()))
let csr_bool = $(CCSR.G.to_bq (CCSR.bool_g ()))


let show_int = Int.to_string
let show_float = Float.to_string
let show_bool = Bool.to_string
let show_pair (a, b) = Printf.sprintf "(%d,%d)" a b
let show_list f l = "[" ^ String.concat ";" (List.map f l) ^ "]"

let eq_pair (a, b) (c, d) = a = c && b = d

let t ~name ~show ~equal g1 g2 =
  pair_test ~config:qc_cfg ~name ~show ~equal g1 g2

let int_rows name bq sr c csr =
  [ t ~name:(name ^ " SR") ~show:show_int ~equal:Int.equal bq sr;
    t ~name:(name ^ " C") ~show:show_int ~equal:Int.equal bq c;
    t ~name:(name ^ " CSR") ~show:show_int ~equal:Int.equal bq csr ]

let () =
  Difftest.run_suites
    [ ( "Effect ordering / choose (SR)",
        [ t ~name:"bind order" ~show:show_pair ~equal:eq_pair bq_bind_order
            sr_bind_order;
          t ~name:"choose" ~show:show_int ~equal:Int.equal bq_choose sr_choose;
          t ~name:"choose bind1" ~show:show_int ~equal:Int.equal
            bq_choose_bind1 sr_choose_bind1;
          t ~name:"choose bind2" ~show:show_pair ~equal:eq_pair
            bq_choose_bind2 sr_choose_bind2
        ] );
      ( "List combinators (SR)",
        [ t ~name:"const false list"
            ~show:(show_list show_bool)
            ~equal:(List.equal Bool.equal)
            bq_const_false_list sr_const_false_list;
          t ~name:"size list"
            ~show:(show_list show_int)
            ~equal:(List.equal Int.equal)
            bq_size_list sr_size_list;
          t ~name:"bool list"
            ~show:(show_list show_bool)
            ~equal:(List.equal Bool.equal)
            bq_bool_list sr_bool_list;
          t ~name:"bool list list"
            ~show:(show_list (show_list show_bool))
            ~equal:(List.equal (List.equal Bool.equal))
            bq_bool_list_list sr_bool_list_list
        ] );
      ("int", int_rows "int" bq_int sr_int c_int csr_int);
      ( "int_uniform",
        int_rows "int_uniform" bq_int_uniform sr_int_uniform c_int_uniform
          csr_int_uniform );
      ( "int_uniform_inclusive",
        int_rows "iui" bq_int_uniform_inclusive sr_int_uniform_inclusive
          c_int_uniform_inclusive csr_int_uniform_inclusive );
      ( "int_inclusive",
        int_rows "ii" bq_int_inclusive sr_int_inclusive c_int_inclusive
          csr_int_inclusive );
      ( "int_log_uniform_inclusive",
        int_rows "ilui" bq_int_log_uniform_inclusive
          sr_int_log_uniform_inclusive c_int_log_uniform_inclusive
          csr_int_log_uniform_inclusive );
      ( "int_log_inclusive",
        int_rows "ili" bq_int_log_inclusive sr_int_log_inclusive
          c_int_log_inclusive csr_int_log_inclusive );
      ( "floats",
        [ t ~name:"float ex SR" ~show:show_float ~equal:Float.equal bq_float_ex
            sr_float_ex;
          t ~name:"float ex C" ~show:show_float ~equal:Float.equal bq_float_ex
            c_float_ex;
          t ~name:"float ex CSR" ~show:show_float ~equal:Float.equal
            bq_float_ex csr_float_ex;
          t ~name:"float in SR" ~show:show_float ~equal:Float.equal bq_float_in
            sr_float_in;
          t ~name:"float in C" ~show:show_float ~equal:Float.equal bq_float_in
            c_float_in;
          t ~name:"float in CSR" ~show:show_float ~equal:Float.equal
            bq_float_in csr_float_in
        ] );
      ( "bool",
        [ t ~name:"bool SR" ~show:show_bool ~equal:Bool.equal bq_bool sr_bool;
          t ~name:"bool C" ~show:show_bool ~equal:Bool.equal bq_bool c_bool;
          t ~name:"bool CSR" ~show:show_bool ~equal:Bool.equal bq_bool csr_bool
        ] );
      ( "Strategies (staged SR vs unstaged, same seeds)",
        [ t ~name:"boollist"
            ~show:(show_list show_bool)
            ~equal:(List.equal Bool.equal)
            BoolList.Boollist.quickcheck_generator
            BoolList.Boollist_staged_sr.quickcheck_generator;
          t ~name:"bst bespoke"
            ~show:(fun t -> Sexplib0.Sexp.to_string (BST.Bst_baseBespoke.sexp_of_t t))
            ~equal:Stdlib.( = )
            BST.Bst_baseBespoke.quickcheck_generator
            BST.Bst_baseBespoke_Staged_SR.quickcheck_generator;
          t ~name:"bst single-pass"
            ~show:(fun t -> Sexplib0.Sexp.to_string (BST.Bst_baseSingleBespoke.sexp_of_t t))
            ~equal:Stdlib.( = )
            BST.Bst_baseSingleBespoke.quickcheck_generator
            BST.Bst_baseSingleBespoke_Staged_SR.quickcheck_generator;
          t ~name:"stlc bespoke"
            ~show:(fun t -> Sexplib0.Sexp.to_string (STLC.BaseBespoke.sexp_of_t t))
            ~equal:Stdlib.( = )
            STLC.BaseBespoke.quickcheck_generator
            STLC.BaseBespoke_Staged_SR.quickcheck_generator;
          t ~name:"bst type-derived"
            ~show:(fun t -> Sexplib0.Sexp.to_string (BST.Bst_baseType.sexp_of_t t))
            ~equal:Stdlib.( = )
            BST.Bst_baseType.quickcheck_generator
            BST.Bst_baseType_Staged_SR.quickcheck_generator;
          t ~name:"stlc type-derived"
            ~show:(fun t -> Sexplib0.Sexp.to_string (STLC.BaseType.sexp_of_t t))
            ~equal:Stdlib.( = )
            STLC.BaseType.quickcheck_generator
            STLC.BaseType_Staged_SR.quickcheck_generator
        ] )
    ]
