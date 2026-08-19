type t = C_random_runtime.t

macro int : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi ->
  << if Base.Int.compare $lo $hi > 0 then failwith "Crossed bounds!"
     else C_random_runtime.int_c_unchecked $st $lo $hi >>

macro int_unchecked : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi -> << C_random_runtime.int_c_unchecked $st $lo $hi >>

macro bool : t expr -> bool expr = fun st -> << C_random_runtime.bool_c $st >>

macro float : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi ->
  << if Base.Float.compare $lo $hi > 0 then failwith "Crossed bounds!"
     else if not (Float.is_finite $lo && Float.is_finite $hi) then
       failwith "Infite floats"
     else C_random_runtime.float_c_unchecked $st $lo $hi >>

macro float_unchecked : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi -> << C_random_runtime.float_c_unchecked $st $lo $hi >>

macro one_ulp : dir:[ `Up | `Down ] -> float expr -> float expr =
  fun ~dir x ->
  match dir with
  | `Up -> << C_random_runtime.one_ulp_up $x >>
  | `Down -> << C_random_runtime.one_ulp_down $x >>

module Log_uniform = struct
  macro int : t expr -> lo:int expr -> hi:int expr -> int expr =
    fun st ~lo ~hi -> << C_random_runtime.int_c_log_uniform $st $lo $hi >>
end

macro of_sr : Splittable_random.t expr -> t expr = fun sr_t ->
  << let o = Obj.repr $sr_t in
     let seed : int64 = Obj.obj (Obj.field o 0) in
     let odd_gamma : int64 = Obj.obj (Obj.field o 1) in
     C_random_runtime.create seed odd_gamma >>

macro repopulate_sr : t expr -> Splittable_random.t expr -> unit expr =
  fun t t_sr -> << C_random_runtime.repopulate $t $t_sr >>
