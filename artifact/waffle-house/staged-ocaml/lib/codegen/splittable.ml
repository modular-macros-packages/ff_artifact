module type S = sig
  type t
  type f

  $val split : t expr -> f Codecps.t
end
