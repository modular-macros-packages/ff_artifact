type t = Splittable_random.t

$gen int : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi -> << Splittable_random.int $st ~lo:$lo ~hi:$hi >>

$gen int_unchecked : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi -> << Splittable_random.int $st ~lo:$lo ~hi:$hi >>

$gen bool : t expr -> bool expr = fun st -> << Splittable_random.bool $st >>

$gen float : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi -> << Splittable_random.float $st ~lo:$lo ~hi:$hi >>

$gen float_unchecked : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi -> << Splittable_random.float $st ~lo:$lo ~hi:$hi >>

$gen one_ulp : dir:[ `Up | `Down ] -> float expr -> float expr =
  fun ~dir x ->
  match dir with
  | `Up -> << Base.Float.one_ulp `Up $x >>
  | `Down -> << Base.Float.one_ulp `Down $x >>

module Log_uniform = struct
  $gen int : t expr -> lo:int expr -> hi:int expr -> int expr =
    fun st ~lo ~hi -> << Splittable_random.Log_uniform.int $st ~lo:$lo ~hi:$hi >>
end

$gen of_sr : Splittable_random.t expr -> t expr = fun x -> x

$gen repopulate_sr : t expr -> Splittable_random.t expr -> unit expr =
  fun _ _ -> << () >>
