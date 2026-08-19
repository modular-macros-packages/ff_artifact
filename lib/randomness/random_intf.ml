module type S = sig
  type t

  macro bool : t expr -> bool expr
  macro int : t expr -> lo:int expr -> hi:int expr -> int expr
  macro int_unchecked : t expr -> lo:int expr -> hi:int expr -> int expr
  macro float : t expr -> lo:float expr -> hi:float expr -> float expr

  macro float_unchecked :
    t expr -> lo:float expr -> hi:float expr -> float expr

  macro one_ulp : dir:[ `Up | `Down ] -> float expr -> float expr

  module Log_uniform : sig
    macro int : t expr -> lo:int expr -> hi:int expr -> int expr
  end

  macro of_sr : Splittable_random.t expr -> t expr

  macro repopulate_sr :
    t expr -> Splittable_random.t expr -> unit expr
end
