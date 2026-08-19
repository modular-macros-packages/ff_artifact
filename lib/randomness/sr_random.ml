type t = Splittable_random.t

macro int : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi -> << Splittable_random.int $st ~lo:$lo ~hi:$hi >>

macro int_unchecked : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi -> << Splittable_random.int $st ~lo:$lo ~hi:$hi >>

macro bool : t expr -> bool expr = fun st -> << Splittable_random.bool $st >>

macro float : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi -> << Splittable_random.float $st ~lo:$lo ~hi:$hi >>

macro float_unchecked : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi -> << Splittable_random.float $st ~lo:$lo ~hi:$hi >>

macro one_ulp : dir:[ `Up | `Down ] -> float expr -> float expr =
  fun ~dir x ->
  match dir with
  | `Up -> << Base.Float.one_ulp `Up $x >>
  | `Down -> << Base.Float.one_ulp `Down $x >>

module Log_uniform = struct
  macro int : t expr -> lo:int expr -> hi:int expr -> int expr =
    fun st ~lo ~hi -> << Splittable_random.Log_uniform.int $st ~lo:$lo ~hi:$hi >>
end

macro of_sr : Splittable_random.t expr -> t expr = fun x -> x

macro repopulate_sr : t expr -> Splittable_random.t expr -> unit expr =
  fun _ _ -> << () >>
