
type 'a t = { code_gen : 'z. ('a -> 'z expr) -> 'z expr }

$let error : 'a. string -> 'a t = fun s ->
  { code_gen = (fun _ -> << failwith $(Expr.string s) >>) }

$let v2c : 'a. 'a expr -> 'a expr = fun x -> x

$let run_code_gen : 'a 'z. 'a t -> ('a -> 'z expr) -> 'z expr =
  fun { code_gen = f } k -> f k

$let code_generate : 'a. 'a expr t -> 'a expr =
  fun g -> g.code_gen (fun x -> x)

$let return : 'a. 'a -> 'a t = fun x -> { code_gen = (fun k -> k x) }

$let bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
  fun { code_gen = inC } f ->
  { code_gen = (fun k -> inC (fun a -> run_code_gen (f a) k)) }

$let all : 'a. 'a t list -> 'a list t = fun xs ->
  let rec go = function
    | [] -> return []
    | cx :: cxs ->
        bind cx @@ fun x ->
        bind (go cxs) @@ fun xs -> return (x :: xs)
  in
  go xs

$let let_insert : 'a. 'a expr -> 'a expr t = fun cx ->
  { code_gen = (fun k -> << let x = $cx in $(k << x >>) >>) }

$let seq_insert : unit expr -> unit t = fun cx ->
  { code_gen = (fun k -> << $cx; $(k ()) >>) }

$let let_insert_smart : 'a. 'a expr -> 'a expr t = fun cx -> let_insert cx

$let let_insertv : 'a. 'a expr -> 'a expr t = fun cx -> let_insert cx

$let split_bool : bool expr -> bool t = fun cb ->
  { code_gen =
      (fun k ->
        << let b = $cb in if b then $(k true) else $(k false) >>)
  }

$let split_int : int expr -> [ `Z | `S of int expr ] t = fun cn ->
  { code_gen =
      (fun k ->
        << let n = $cn in
           if n = 0 then $(k `Z) else $(k (`S << pred n >>)) >>)
  }

$let split_index : int expr -> int -> int t = fun cn n ->
  if n < 1 then invalid_arg "Codecps.split_index: no cases";
  { code_gen =
      (fun k ->
        if n = 1 then << let _ = $cn in $(k 0) >>
        else
          Expr.switch (module Int) cn ~numconsts:n
            (List.init n (fun j -> (j, k j))) None)
  }

$let split_option : 'a. 'a option expr -> [ `None | `Some of 'a expr ] t =
  fun cb ->
  { code_gen =
      (fun k ->
        << match $cb with
           | None -> $(k `None)
           | Some x -> $(k (`Some << x >>)) >>)
  }

$let split_pair : 'a 'b. ('a * 'b) expr -> ('a expr * 'b expr) t = fun cp ->
  { code_gen =
      (fun k -> << let a, b = $cp in $(k (<< a >>, << b >>)) >>)
  }

$let split_triple :
  'a 'b 'c. ('a * 'b * 'c) expr -> ('a expr * 'b expr * 'c expr) t =
  fun ct ->
  { code_gen =
      (fun k -> << let a, b, c = $ct in $(k (<< a >>, << b >>, << c >>)) >>)
  }

$let split_list :
  'a. 'a list expr -> [ `Nil | `Cons of 'a expr * 'a list expr ] t =
  fun cxs ->
  { code_gen =
      (fun k ->
        << match $cxs with
           | [] -> $(k `Nil)
           | x :: xs -> $(k (`Cons (<< x >>, << xs >>))) >>)
  }
