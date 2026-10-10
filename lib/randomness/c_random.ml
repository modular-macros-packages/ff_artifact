type t = C_random_runtime.t

$let int : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi ->
  << if Base.Int.compare $lo $hi > 0 then failwith "Crossed bounds!"
     else C_random_runtime.int_c_unchecked $st $lo $hi >>

$let int_unchecked : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi -> << C_random_runtime.int_c_unchecked $st $lo $hi >>

$let bool : t expr -> bool expr = fun st -> << C_random_runtime.bool_c $st >>

$let float : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi ->
  << if Base.Float.compare $lo $hi > 0 then failwith "Crossed bounds!"
     else if not (Float.is_finite $lo && Float.is_finite $hi) then
       failwith "Infite floats"
     else C_random_runtime.float_c_unchecked $st $lo $hi >>

$let float_unchecked : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi -> << C_random_runtime.float_c_unchecked $st $lo $hi >>

$let one_ulp : dir:[ `Up | `Down ] -> float expr -> float expr =
  fun ~dir x ->
  match dir with
  | `Up -> << C_random_runtime.one_ulp_up $x >>
  | `Down -> << C_random_runtime.one_ulp_down $x >>

module Log_uniform = struct
  $let int : t expr -> lo:int expr -> hi:int expr -> int expr =
    fun st ~lo ~hi -> << C_random_runtime.int_c_log_uniform $st $lo $hi >>
end

$let of_sr : Splittable_random.t expr -> t expr = fun sr_t ->
  << let o = Obj.repr $sr_t in
     let seed : int64 = Obj.obj (Obj.field o 0) in
     let odd_gamma : int64 = Obj.obj (Obj.field o 1) in
     C_random_runtime.create seed odd_gamma >>

$let repopulate_sr : t expr -> Splittable_random.t expr -> unit expr =
  fun t t_sr -> << C_random_runtime.repopulate $t $t_sr >>
