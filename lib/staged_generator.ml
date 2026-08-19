
module MakeStaged [R : Random_intf.S] = struct
  type 'a t =
    { rand_gen : size_c:int expr -> random_c:R.t expr -> 'a Codecps.t }

  
  macro split_bool : bool expr -> bool t = fun cb ->
    { rand_gen = (fun ~size_c:_ ~random_c:_ -> Codecps.split_bool cb) }

  macro split_int : int expr -> [ `Z | `S of int expr ] t = fun cn ->
    { rand_gen = (fun ~size_c:_ ~random_c:_ -> Codecps.split_int cn) }

  macro split_pair : 'a 'b. ('a * 'b) expr -> ('a expr * 'b expr) t = fun cp ->
    { rand_gen = (fun ~size_c:_ ~random_c:_ -> Codecps.split_pair cp) }

  macro split_triple :
    'a 'b 'c. ('a * 'b * 'c) expr -> ('a expr * 'b expr * 'c expr) t =
    fun ct ->
    { rand_gen = (fun ~size_c:_ ~random_c:_ -> Codecps.split_triple ct) }

  macro split_list :
    'a. 'a list expr -> [ `Nil | `Cons of 'a expr * 'a list expr ] t =
    fun cxs ->
    { rand_gen = (fun ~size_c:_ ~random_c:_ -> Codecps.split_list cxs) }

  macro split_option : 'a. 'a option expr -> [ `None | `Some of 'a expr ] t =
    fun cxs ->
    { rand_gen = (fun ~size_c:_ ~random_c:_ -> Codecps.split_option cxs) }

  module MakeSplit [X : Splittable.S] = struct
    macro split : X.t expr -> X.f t = fun cx ->
      { rand_gen = (fun ~size_c:_ ~random_c:_ -> X.split cx) }
  end

  module C = struct
    type 'a t = 'a expr

    macro lift : 'a. 'a -> 'a t = fun x -> Expr.unsafe_lift_constant x
    macro pair : 'a 'b. 'a t -> 'b t -> ('a * 'b) t =
      fun x y -> << ($x, $y) >>
    macro i2f : int t -> float t = fun x -> << Float.of_int $x >>
    macro pred : int t -> int t = fun n -> << $n - 1 >>
    macro div2 : int t -> int t = fun n -> << $n / 2 >>
    macro cons : 'a. 'a t -> 'a list t -> 'a list t =
      fun x xs -> << $x :: $xs >>
    macro cnil : unit -> 'a list t = fun () -> << [] >>
    macro plus : int t -> int t -> int t = fun m n -> << $m + $n >>
    macro minus : int t -> int t -> int t = fun m n -> << $m - $n >>
    macro modulus : int t -> int -> int t =
      fun x n -> << $x mod $(Expr.int n) >>
    macro modulus2 : int t -> int t -> int t = fun x1 x2 -> << $x1 mod $x2 >>
  end

  type 'a c = 'a C.t

  
  macro return : 'a. 'a -> 'a t =
    fun x -> { rand_gen = (fun ~size_c:_ ~random_c:_ -> Codecps.return x) }

  macro bind : 'a 'b. 'a t -> f:('a -> 'b t) -> 'b t = fun r ~f ->
    { rand_gen =
        (fun ~size_c ~random_c ->
          Codecps.bind (r.rand_gen ~size_c ~random_c) (fun a ->
              (f a).rand_gen ~size_c ~random_c))
    }

  macro map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t =
    fun x ~f -> bind x ~f:(fun cx -> return (f cx))

  macro apply : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t =
    fun f x -> bind f ~f:(fun f -> bind x ~f:(fun x -> return (f x)))

  macro join : 'a. 'a t t -> 'a t = fun g -> bind g ~f:(fun x -> x)

  macro both : 'a 'b. 'a t -> 'b t -> ('a * 'b) t =
    fun a b -> bind a ~f:(fun a -> bind b ~f:(fun b -> return (a, b)))

  macro map2 : 'a 'b 'r. 'a t -> 'b t -> f:('a -> 'b -> 'r) -> 'r t =
    fun a b ~f -> bind a ~f:(fun a -> map b ~f:(fun b -> f a b))

  macro map3 :
    'a 'b 'c 'r. 'a t -> 'b t -> 'c t -> f:('a -> 'b -> 'c -> 'r) -> 'r t =
    fun a b c ~f ->
    bind a ~f:(fun a -> bind b ~f:(fun b -> map c ~f:(fun c -> f a b c)))

  macro all : 'a. 'a t list -> 'a list t = fun xs ->
    let rec go = function
      | [] -> return []
      | g :: gs -> bind g ~f:(fun x -> map (go gs) ~f:(fun xs -> x :: xs))
    in
    go xs

  macro ( >>= ) : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t = fun x f -> bind x ~f
  macro ( >>| ) : 'a 'b. 'a t -> ('a -> 'b) -> 'b t = fun x f -> map x ~f
  macro ( <*> ) : 'a 'b. ('a -> 'b) t -> 'a t -> 'b t = fun f x -> apply f x

  module Let_syntax = struct
    macro return : 'a. 'a -> 'a t = fun x -> return x
    macro bind : 'a 'b. 'a t -> f:('a -> 'b t) -> 'b t = fun x ~f -> bind x ~f
    macro map : 'a 'b. 'a t -> f:('a -> 'b) -> 'b t = fun x ~f -> map x ~f
    macro both : 'a 'b. 'a t -> 'b t -> ('a * 'b) t = fun a b -> both a b
  end

  
  macro bool : unit -> bool expr t = fun () ->
    { rand_gen =
        (fun ~size_c:_ ~random_c -> Codecps.let_insert (R.bool random_c))
    }

  macro float_uniform_exclusive :
    lo:float expr -> hi:float expr -> float expr t = fun ~lo ~hi ->
    { rand_gen =
        (fun ~size_c:_ ~random_c ->
          Codecps.bind (Codecps.let_insert (R.one_ulp ~dir:`Up lo))
          @@ fun lo_incl ->
          Codecps.bind (Codecps.let_insert (R.one_ulp ~dir:`Down hi))
          @@ fun hi_incl ->
          Codecps.let_insert (R.float random_c ~lo:lo_incl ~hi:hi_incl))
    }

  macro float_uniform_inclusive :
    lo:float expr -> hi:float expr -> float expr t = fun ~lo ~hi ->
    { rand_gen =
        (fun ~size_c:_ ~random_c ->
          Codecps.let_insert (R.float random_c ~lo ~hi))
    }

  macro genpick : 'a. float expr -> (float expr * 'a t) list -> 'a t =
    fun n ws ->
    let rec go n ws =
      match ws with
      | [] ->
          { rand_gen =
              (fun ~size_c:_ ~random_c:_ ->
                Codecps.error "Fell of the end of pick list")
          }
      | (k, g) :: ws' ->
          { rand_gen =
              (fun ~size_c ~random_c ->
                Codecps.bind
                  (Codecps.split_bool << Float.compare $n $k <= 0 >>)
                  (fun leq ->
                    if leq then g.rand_gen ~size_c ~random_c
                    else
                      Codecps.bind (Codecps.let_insertv << $n -. $k >>)
                      @@ fun n' -> (go n' ws').rand_gen ~size_c ~random_c))
          }
    in
    go n ws

  macro sum :
    'a. (float expr * 'a t) list -> float expr Codecps.t = fun ws ->
    let rec go ws acc =
      match ws with
      | [] -> Codecps.return acc
      | (cn, _) :: ws' -> go ws' << $acc +. $cn >>
    in
    go ws << 0. >>

  macro weighted_union : 'a. (float expr * 'a t) list -> 'a t = fun ws ->
    { rand_gen =
        (fun ~size_c ~random_c ->
          Codecps.bind
            (Codecps.all
            @@ List.map
                 (fun (cn, g) ->
                   Codecps.bind (Codecps.let_insertv cn) @@ fun cvn ->
                   Codecps.return (cvn, g))
                 ws)
          @@ fun ws' ->
          Codecps.bind (sum ws') @@ fun s ->
          Codecps.bind (Codecps.let_insertv s) @@ fun sum' ->
          let n = R.float random_c ~lo:<< 0. >> ~hi:sum' in
          Codecps.bind (Codecps.let_insertv n) @@ fun n ->
          (genpick n ws').rand_gen ~size_c ~random_c)
    }

  macro int_uniform_inclusive : lo:int expr -> hi:int expr -> int expr t =
    fun ~lo ~hi ->
    { rand_gen =
        (fun ~size_c:_ ~random_c ->
          Codecps.let_insert (R.int random_c ~lo ~hi))
    }

  macro int_log_uniform_inclusive : lo:int expr -> hi:int expr -> int expr t =
    fun ~lo ~hi ->
    { rand_gen =
        (fun ~size_c:_ ~random_c ->
          Codecps.let_insert (R.Log_uniform.int random_c ~lo ~hi))
    }

  macro non_uniform :
    (lo:int expr -> hi:int expr -> int expr t) ->
    lo:int expr -> hi:int expr -> int expr t = fun f ~lo ~hi ->
    { rand_gen =
        (fun ~size_c ~random_c ->
          let n = R.float_unchecked random_c ~lo:<< 0. >> ~hi:<< 1.0 >> in
          Codecps.bind (Codecps.let_insertv n) @@ fun n ->
          Codecps.bind
            (Codecps.split_bool << Float.compare $n 0.05 <= 0 >>)
          @@ fun leq_first ->
          if leq_first then Codecps.return lo
          else
            Codecps.bind
              (Codecps.split_bool << Float.compare $n 0.1 <= 0 >>)
            @@ fun leq_second ->
            if leq_second then Codecps.return hi
            else (f ~lo ~hi).rand_gen ~size_c ~random_c)
    }

  macro int_inclusive : lo:int expr -> hi:int expr -> int expr t =
    fun ~lo ~hi -> non_uniform int_uniform_inclusive ~lo ~hi

  macro int_log_inclusive : lo:int expr -> hi:int expr -> int expr t =
    fun ~lo ~hi -> non_uniform int_log_uniform_inclusive ~lo ~hi

  macro uniform_all : unit -> int expr t = fun () ->
    int_uniform_inclusive ~lo:<< Int.min_int >> ~hi:<< Int.max_int >>

  macro int_uniform : unit -> int expr t = fun () -> uniform_all ()

  macro int : unit -> int expr t = fun () ->
    bind (bool ()) ~f:(fun negative ->
        bind
          (int_log_inclusive ~lo:<< 0 >> ~hi:<< Base.Int.max_value >>)
          ~f:(fun magnitude ->
            return << $magnitude lxor (-Base.Bool.to_int $negative) >>))

  macro of_list : 'a. 'a list -> 'a t = fun xs ->
    let n = List.length xs - 1 in
    let rec go i xs =
      match xs with
      | [] -> failwith "empty list!"
      | [ x ] -> return x
      | x :: xs ->
          { rand_gen =
              (fun ~size_c ~random_c ->
                Codecps.bind (Codecps.split_bool << $i == 0 >>) @@ fun b ->
                if b then Codecps.return x
                else
                  Codecps.bind (Codecps.let_insert << $i - 1 >>)
                  @@ fun i_pred -> (go i_pred xs).rand_gen ~size_c ~random_c)
          }
    in
    bind
      (int_uniform_inclusive ~lo:<< 0 >> ~hi:(Expr.int n))
      ~f:(fun i -> go i xs)

  macro union : 'a. 'a t list -> 'a t = fun xs -> join (of_list xs)

  macro of_list_dyn : 'a. 'a list expr -> 'a expr t = fun cxs ->
    { rand_gen =
        (fun ~size_c ~random_c ->
                    Codecps.bind
            (Codecps.let_insert
               << let rec go xs i =
                    match xs with
                    | [] -> failwith "Impossible"
                    | y :: ys -> if i == 0 then y else go ys (i - 1)
                  in
                  go >>)
          @@ fun loop ->
          Codecps.bind (Codecps.let_insert cxs) @@ fun cxs ->
          Codecps.bind (Codecps.let_insert << List.length $cxs - 1 >>)
          @@ fun n ->
          Codecps.bind
            ((int_uniform_inclusive ~lo:<< 0 >> ~hi:n).rand_gen ~size_c
               ~random_c)
          @@ fun i ->
          Codecps.return
            << if $n < 0 then failwith "of_list_dn passed empty list"
               else $loop $cxs $i >>)
    }

  macro with_size : 'a. 'a t -> size_c:int expr -> 'a t = fun f ~size_c ->
    { rand_gen =
        (fun ~size_c:_ ~random_c -> f.rand_gen ~size_c ~random_c)
    }

  macro size : unit -> int expr t = fun () ->
    { rand_gen = (fun ~size_c ~random_c:_ -> Codecps.return size_c) }

  macro list : 'a. 'a expr t -> 'a list expr t = fun g ->
    let go len =
      { rand_gen =
          (fun ~size_c ~random_c ->
            Codecps.bind (Codecps.let_insert << Array.make $len 0 >>)
            @@ fun sizes ->
            Codecps.bind (Codecps.let_insert << $size_c - $len >>)
            @@ fun remaining ->
            Codecps.bind (Codecps.let_insert << $len - 1 >>)
            @@ fun max_index ->
            Codecps.bind
              (Codecps.seq_insert
                 << for _ = 1 to $remaining do
                      let i =
                        $(R.Log_uniform.int random_c ~lo:<< 0 >> ~hi:max_index)
                      in
                      ($sizes).(i) <- ($sizes).(i) + 1
                    done >>)
            @@ fun () ->
            Codecps.bind
              (Codecps.seq_insert
                 << for i = 0 to $max_index - 1 do
                      let j =
                        $(R.int random_c ~lo:<< i >> ~hi:max_index)
                      in
                      let tmp = ($sizes).(i) in
                      ($sizes).(i) <- ($sizes).(j);
                      ($sizes).(j) <- tmp
                    done >>)
            @@ fun () ->
            Codecps.bind
              (Codecps.let_insert_smart
                 << fun sz ->
                      $(Codecps.code_generate
                          (g.rand_gen ~size_c:<< sz >> ~random_c)) >>)
            @@ fun f ->
            Codecps.return << List.map $f (Array.to_list $sizes) >>)
      }
    in
    bind (size ()) ~f:(fun sz ->
        bind (int_log_uniform_inclusive ~lo:<< 0 >> ~hi:sz) ~f:(fun len ->
            bind (split_int len) ~f:(fun slen ->
                match slen with `Z -> return << [] >> | `S _ -> go len)))

  macro if_z : 'a. int expr -> 'a t -> 'a t -> 'a t = fun cx gz gsucc ->
    bind (split_int cx) ~f:(function `Z -> gz | `S _ -> gsucc)

  macro to_bq : 'a. 'a expr t -> 'a Base_quickcheck.Generator.t expr =
    fun sg ->
    << Base_quickcheck.Generator.create (fun ~size ~random ->
           let custom_random = $(R.of_sr << random >>) in
           let v =
             $(Codecps.code_generate
                 (sg.rand_gen ~size_c:<< size >> ~random_c:<< custom_random >>))
           in
           $(R.repopulate_sr << custom_random >> << random >>);
           v) >>

  type ('a, 'r) recgen = 'r expr -> 'a expr t

  macro recurse : 'a 'r. ('a, 'r) recgen -> 'r expr -> 'a expr t = fun f x ->
    { rand_gen =
        (fun ~size_c ~random_c ->
          Codecps.bind ((f x).rand_gen ~size_c ~random_c) @@ fun c ->
          Codecps.let_insert c)
    }

  macro recursive :
    'a 'r. 'r expr -> (('a, 'r) recgen -> 'r expr -> 'a expr t) -> 'a expr t =
    fun x0 step ->
    { rand_gen =
        (fun ~size_c ~random_c ->
          Codecps.bind (Codecps.let_insertv x0) @@ fun x0 ->
          Codecps.let_insert
            << let rec go x ~size ~random =
                 $(Codecps.code_generate
                     ((step
                         (fun xc' ->
                           { rand_gen =
                               (fun ~size_c ~random_c ->
                                 Codecps.return
                                   << go $xc' ~size:$size_c ~random:$random_c >>)
                           })
                         << x >>)
                        .rand_gen ~size_c:<< size >> ~random_c:<< random >>))
               in
               go $x0 ~size:$size_c ~random:$random_c >>)
    }
end
