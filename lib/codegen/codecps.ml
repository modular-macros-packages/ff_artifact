
type 'a t = { code_gen : 'z. ('a -> 'z expr) -> 'z expr }

macro error : 'a. string -> 'a t = fun s ->
  { code_gen = (fun _ -> << failwith $(Expr.string s) >>) }

macro v2c : 'a. 'a expr -> 'a expr = fun x -> x

macro run_code_gen : 'a 'z. 'a t -> ('a -> 'z expr) -> 'z expr =
  fun { code_gen = f } k -> f k

macro code_generate : 'a. 'a expr t -> 'a expr =
  fun g -> g.code_gen (fun x -> x)

macro return : 'a. 'a -> 'a t = fun x -> { code_gen = (fun k -> k x) }

macro bind : 'a 'b. 'a t -> ('a -> 'b t) -> 'b t =
  fun { code_gen = inC } f ->
  { code_gen = (fun k -> inC (fun a -> run_code_gen (f a) k)) }

macro all : 'a. 'a t list -> 'a list t = fun xs ->
  let rec go = function
    | [] -> return []
    | cx :: cxs ->
        bind cx @@ fun x ->
        bind (go cxs) @@ fun xs -> return (x :: xs)
  in
  go xs

macro let_insert : 'a. 'a expr -> 'a expr t = fun cx ->
  { code_gen = (fun k -> << let x = $cx in $(k << x >>) >>) }

macro seq_insert : unit expr -> unit t = fun cx ->
  { code_gen = (fun k -> << $cx; $(k ()) >>) }

macro let_insert_smart : 'a. 'a expr -> 'a expr t = fun cx -> let_insert cx

macro let_insertv : 'a. 'a expr -> 'a expr t = fun cx -> let_insert cx

macro split_bool : bool expr -> bool t = fun cb ->
  { code_gen =
      (fun k ->
        << let b = $cb in if b then $(k true) else $(k false) >>)
  }

macro split_int : int expr -> [ `Z | `S of int expr ] t = fun cn ->
  { code_gen =
      (fun k ->
        << let n = $cn in
           if n = 0 then $(k `Z) else $(k (`S << pred n >>)) >>)
  }

macro split_option : 'a. 'a option expr -> [ `None | `Some of 'a expr ] t =
  fun cb ->
  { code_gen =
      (fun k ->
        << match $cb with
           | None -> $(k `None)
           | Some x -> $(k (`Some << x >>)) >>)
  }

macro split_pair : 'a 'b. ('a * 'b) expr -> ('a expr * 'b expr) t = fun cp ->
  { code_gen =
      (fun k -> << let a, b = $cp in $(k (<< a >>, << b >>)) >>)
  }

macro split_triple :
  'a 'b 'c. ('a * 'b * 'c) expr -> ('a expr * 'b expr * 'c expr) t =
  fun ct ->
  { code_gen =
      (fun k -> << let a, b, c = $ct in $(k (<< a >>, << b >>, << c >>)) >>)
  }

macro split_list :
  'a. 'a list expr -> [ `Nil | `Cons of 'a expr * 'a list expr ] t =
  fun cxs ->
  { code_gen =
      (fun k ->
        << match $cxs with
           | [] -> $(k `Nil)
           | x :: xs -> $(k (`Cons (<< x >>, << xs >>))) >>)
  }
