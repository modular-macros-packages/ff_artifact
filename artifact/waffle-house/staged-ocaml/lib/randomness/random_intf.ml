module type S = sig
  type t

  $gen bool : t expr -> bool expr
  $gen int : t expr -> lo:int expr -> hi:int expr -> int expr
  $gen int_unchecked : t expr -> lo:int expr -> hi:int expr -> int expr
  $gen float : t expr -> lo:float expr -> hi:float expr -> float expr

  $gen float_unchecked :
    t expr -> lo:float expr -> hi:float expr -> float expr

  $gen one_ulp : dir:[ `Up | `Down ] -> float expr -> float expr

  module Log_uniform : sig
    $gen int : t expr -> lo:int expr -> hi:int expr -> int expr
  end

  $gen of_sr : Splittable_random.t expr -> t expr

  $gen repopulate_sr :
    t expr -> Splittable_random.t expr -> unit expr
end
