module type S = sig
  type t

  $val bool : t expr -> bool expr
  $val int : t expr -> lo:int expr -> hi:int expr -> int expr
  $val int_unchecked : t expr -> lo:int expr -> hi:int expr -> int expr
  $val float : t expr -> lo:float expr -> hi:float expr -> float expr

  $val float_unchecked :
    t expr -> lo:float expr -> hi:float expr -> float expr

  $val one_ulp : dir:[ `Up | `Down ] -> float expr -> float expr

  module Log_uniform : sig
    $val int : t expr -> lo:int expr -> hi:int expr -> int expr
  end

  $val of_sr : Splittable_random.t expr -> t expr

  $val repopulate_sr :
    t expr -> Splittable_random.t expr -> unit expr
end
