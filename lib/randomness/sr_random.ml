type t = Splittable_random.t

$let int : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi -> << Splittable_random.int $st ~lo:$lo ~hi:$hi >>

$let int_unchecked : t expr -> lo:int expr -> hi:int expr -> int expr =
  fun st ~lo ~hi -> << Splittable_random.int $st ~lo:$lo ~hi:$hi >>

$let bool : t expr -> bool expr = fun st -> << Splittable_random.bool $st >>

$let float : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi -> << Splittable_random.float $st ~lo:$lo ~hi:$hi >>

$let float_unchecked : t expr -> lo:float expr -> hi:float expr -> float expr =
  fun st ~lo ~hi -> << Splittable_random.float $st ~lo:$lo ~hi:$hi >>

$let one_ulp : dir:[ `Up | `Down ] -> float expr -> float expr =
  fun ~dir x ->
  match dir with
  | `Up -> << Base.Float.one_ulp `Up $x >>
  | `Down -> << Base.Float.one_ulp `Down $x >>

module Log_uniform = struct
  $let int : t expr -> lo:int expr -> hi:int expr -> int expr =
    fun st ~lo ~hi -> << Splittable_random.Log_uniform.int $st ~lo:$lo ~hi:$hi >>
end

$let of_sr : Splittable_random.t expr -> t expr = fun x -> x

$let repopulate_sr : t expr -> Splittable_random.t expr -> unit expr =
  fun _ _ -> << () >>
