module type S = sig
  type t
  type f

  $gen split : t expr -> f Codecps.t
end
