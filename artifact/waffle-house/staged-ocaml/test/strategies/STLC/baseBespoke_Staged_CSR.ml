open Fast_gen
open Base
type 'a code = 'a expr
open Type;;

module M : Fast_gen.Splittable.S = struct
  type nonrec t = expr
  type nonrec f = VarF of int code | BoolF of bool code | AbsF of (typ code) * (expr code) | AppF of (expr code) * (expr code)

  $let split : t code -> f Codecps.t = fun e ->
    { code_gen = fun k ->
      << match $e with
      | Var x -> $(k (VarF << x >>))
      | Bool b -> $(k (BoolF << b >>))
      | Abs (t,e') -> $(k (AbsF (<< t >>, << e' >>)))
      | App (e,e') -> $(k (AppF (<< e >>, << e' >>)))
      >>
    }
end

module MT : Fast_gen.Splittable.S with type t = typ and type f = [`TBool | `TFun of (typ code) * (typ code)] = struct
  type nonrec t = typ
  type nonrec f = [`TBool | `TFun of (typ code) * (typ code)]

  $let split : t code -> f Codecps.t = fun e ->
    { code_gen = fun k ->
      << match $e with
      | TBool -> $(k `TBool)
      | TFun (t,t') -> $(k (`TFun (<< t >>, << t' >>)))
      >>
    }
end

module G = Fast_gen.Staged_generator.MakeStaged$(Fast_gen.C_sr_dropin_random)
open G
open Let_syntax
module GS = G.MakeSplit$(M)
module GTS = G.MakeSplit$(MT)

type t = expr [@@deriving quickcheck, sexp]

$let split_expr : M.t code -> M.f G.t = fun e -> GS.split e
$let split_typ : typ code -> MT.f G.t = fun e -> GTS.split e


$let genTyp : typ code G.t =
  recursive << () >> @@ fun go u ->
    let%bind n = size in
    let%bind b = split_bool << $n <= 1 >> in
    if b then return << TBool >>
    else
      weighted_union [
        << 1.0 >>, return << TBool >>;
        << Int.to_float $n >>,
          let%bind t1 = with_size ~size_c:<< $n / 2 >> (recurse go u) in
          let%bind t2 = with_size ~size_c:<< $n / 2 >> (recurse go u) in
          return << TFun($t1,$t2) >>
      ]

$let genConst : typ code -> expr code G.t = fun t ->
    recursive t @@ fun go t ->
      let%bind t = split_typ t in
      match t with
      | `TBool -> map ~f:(fun b -> << Bool $b >>) bool
      | `TFun(t1,t2) -> map ~f:(fun e -> << Abs($t1,$e) >>) (recurse go t2)

$let genVar : typ list code -> typ code -> expr option code G.t = fun g t ->
  let%bind vars = return << List.filter_mapi ~f:(fun i t' -> if Type.equal $t t' then Some (Some (Var i)) else None) $g >> in
  let%bind vars_s = split_list vars in
  match vars_s with
  | `Nil -> return << None >>
  | `Cons _ -> of_list_dyn vars

$let genExactExpr : int code -> typ list code -> typ code -> expr code G.t = fun n g t ->
  recursive << ($n,$g,$t) >> @@ fun go ngt ->
  let%bind (n,g,t) = split_triple ngt in
  let%bind me = (genVar g t) >>= split_option in
  match me with
  | `Some e -> return e
  | `None ->
      let%bind b = split_bool << $n <= 1 >> in
      if b then genConst t else
      let%bind ts = split_typ t in
      match ts with
      | `TFun (t1,t2) -> map ~f:(fun e -> << Abs($t1,$e) >>) (recurse go << ($n - 1,$t1 :: $g,$t2) >>)
      | _ ->
          let%bind t' = genTyp in
          let%bind e1 = recurse go << ($n/2,$g,TFun($t',$t)) >> in
          let%bind e2 = recurse go << ($n/2,$g,$t') >> in
          return << App($e1,$e2) >>

$let genExpr : expr code G.t =
  let%bind n = size in
  let%bind t = genTyp in
  genExactExpr n << [] >> t

let quickcheck_generator = $(G.to_bq genExpr)
let make_quickcheck_generator () = quickcheck_generator

let sexp_of_t = sexp_of_t
