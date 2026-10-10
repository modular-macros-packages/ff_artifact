(* MacoCaml port.  Upstream described MakeStaged by
   [include Generator_intf.S] plus the splitters; that signature's
   items are plain [val]s and it includes Base.Applicative.S and
   Base.Monad.S, so it cannot describe a module whose operations are
   static values ($let).  The staged signature is written out here
   instead, item for item as MakeStaged exports it.  [jit] and [print]
   are gone (there is no runtime JIT): [to_bq] is the entry point,
   consumed by top-level splices. *)

module MakeStaged $(R : Random_intf.S) : sig
  type 'a t =
    { rand_gen : size_c:int expr -> random_c:R.t expr -> 'a Codecps.t }

  (* splitting code values into generation-time cases *)

  $val split_bool : bool expr -> bool t
  $val split_int : int expr -> [ `Z | `S of int expr ] t
  $val split_pair : ('a * 'b) expr -> ('a expr * 'b expr) t
  $val split_triple : ('a * 'b * 'c) expr -> ('a expr * 'b expr * 'c expr) t
  $val split_list : 'a list expr -> [ `Nil | `Cons of 'a expr * 'a list expr ] t
  $val split_option : 'a option expr -> [ `None | `Some of 'a expr ] t

  module MakeSplit $(X : Splittable.S) : sig
    $val split : X.t expr -> X.f t
  end

  (* code combinators *)

  module C : sig
    type 'a t = 'a expr

    $val lift : 'a -> 'a t
    $val pair : 'a t -> 'b t -> ('a * 'b) t
    $val i2f : int t -> float t
    $val pred : int t -> int t
    $val div2 : int t -> int t
    $val cons : 'a t -> 'a list t -> 'a list t
    $val cnil : 'a list t
    $val plus : int t -> int t -> int t
    $val minus : int t -> int t -> int t
    $val modulus : int t -> int -> int t
    $val modulus2 : int t -> int t -> int t
  end

  type 'a c = 'a C.t

  (* monad/applicative surface *)

  $val return : 'a -> 'a t
  $val bind : 'a t -> f:('a -> 'b t) -> 'b t
  $val map : 'a t -> f:('a -> 'b) -> 'b t
  $val apply : ('a -> 'b) t -> 'a t -> 'b t
  $val join : 'a t t -> 'a t
  $val both : 'a t -> 'b t -> ('a * 'b) t
  $val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'r) -> 'r t
  $val map3 : 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'r) -> 'r t
  $val all : 'a t list -> 'a list t
  $val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  $val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  $val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t

  module Let_syntax : sig
    $val return : 'a -> 'a t
    $val bind : 'a t -> f:('a -> 'b t) -> 'b t
    $val map : 'a t -> f:('a -> 'b) -> 'b t
    $val both : 'a t -> 'b t -> ('a * 'b) t
  end

  (* primitive generators *)

  $val bool : bool expr t
  $val float_uniform_exclusive : lo:float expr -> hi:float expr -> float expr t
  $val float_uniform_inclusive : lo:float expr -> hi:float expr -> float expr t

  (* [genpick] and [sum] are [weighted_union]'s helpers *)
  $val genpick : float expr -> (float expr * 'a t) list -> 'a t
  $val sum : (float expr * 'a t) list -> float expr Codecps.t
  $val weighted_union : (float expr * 'a t) list -> 'a t

  $val int_uniform_inclusive : lo:int expr -> hi:int expr -> int expr t
  $val int_log_uniform_inclusive : lo:int expr -> hi:int expr -> int expr t

  $val non_uniform :
    (lo:int expr -> hi:int expr -> int expr t) ->
    lo:int expr -> hi:int expr -> int expr t

  $val int_inclusive : lo:int expr -> hi:int expr -> int expr t
  $val int_log_inclusive : lo:int expr -> hi:int expr -> int expr t
  $val uniform_all : int expr t
  $val int_uniform : int expr t
  $val int : int expr t

  $val of_list : 'a list -> 'a t
  $val union : 'a t list -> 'a t
  $val of_list_dyn : 'a list expr -> 'a expr t

  $val with_size : 'a t -> size_c:int expr -> 'a t
  $val size : int expr t
  $val list : 'a expr t -> 'a list expr t
  $val if_z : int expr -> 'a t -> 'a t -> 'a t

  $val to_bq : 'a expr t -> 'a Base_quickcheck.Generator.t expr

  (* recursive generators *)

  type ('a, 'r) recgen = 'r expr -> 'a expr t

  $val recurse : ('a, 'r) recgen -> 'r expr -> 'a expr t
  $val recursive : 'r expr -> (('a, 'r) recgen -> 'r expr -> 'a expr t) -> 'a expr t
end
