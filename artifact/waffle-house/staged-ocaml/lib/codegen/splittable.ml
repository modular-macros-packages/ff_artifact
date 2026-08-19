module type S = sig
  type t
  type f

  macro split : t expr -> f Codecps.t
end
