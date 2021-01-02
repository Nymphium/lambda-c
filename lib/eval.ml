open Syntax

module Value = struct
  type t =
    | Nil
    | Int of int
    | Fun of string * Term.t
    | Var of string
    | Label of string
  [@@deriving show { with_path = false }]

  let to_term = function
    | Nil -> Term.Nil
    | Int v -> Term.Int v
    | Fun (x, e) -> Term.Fun (x, e)
    | Var x -> Term.Var x
    | Label l -> Term.Label l
  ;;

  let from_term = function
    | Term.Nil -> Nil
    | Term.Int i -> Int i
    | Term.Fun (x, e) -> Fun (x, e)
    | Term.Var x -> Var x
    | Term.Label l -> Label l
    | other ->
      failwith (Printf.sprintf "%s cannot be converted into value" @@ Term.show other)
  ;;
end

(**
  * environment θ for variables and labels
  * each variables / labels is unique and global
  *)
module Store = struct
  open struct
    module M = Map.Make ((String : Map.OrderedType))

    let s : Value.t M.t ref = ref M.empty
  end

  (** θ(x) *)
  let get x = M.find x !s

  (** θ[x <- v] *)
  let set x v = s := M.add x v !s

  let flush x = s := M.add x Value.Nil !s
  let reflesh () = s := M.empty
end

include struct
  open Term

  open struct
    let gensym = Utils.gengensym "r"
    let genlabel = Utils.gengensym "l"

    (** create evaluation context; always left-to-right *)
    let rec punch = function
      | Let (x, bnd, body) when not @@ is_value bnd ->
        let bnd', c = punch bnd in
        bnd', fun hole -> Let (x, c hole, body)
      (* binary operators *)
      | App (App ((PrimOp _ as op), l), r) when not @@ is_value l ->
        let l', c = punch l in
        l', fun hole -> App (App (op, c hole), r)
      | App (App ((PrimOp _ as op), l), r) when not @@ is_value r ->
        let r', c = punch r in
        r', fun hole -> App (App (op, l), c hole)
        (* avoid to be recognized as usual function application *)
      | App (App (PrimOp _, l), r) as t when is_value l && is_value r -> t, Fun.id
      | App (f, e) when not @@ is_value f ->
        let f', c = punch f in
        f', fun hole -> App (c hole, e)
      | Create t when not @@ is_value t ->
        let t', c = punch t in
        t', fun hole -> Create (c hole)
      | Resume (l, e) when not @@ is_value l ->
        let l', c = punch l in
        l', fun hole -> Resume (c hole, e)
      | Yield e when not @@ is_value e ->
        let e', c = punch e in
        e', fun hole -> Yield (c hole)
      (* don't punch `labelE` to avoid `Yield` to be "top-level" *)
      | other -> other, Fun.id
    ;;

    let token_to_arith = function
      | "+" -> ( + )
      | "-" -> ( - )
      | token -> failwith @@ Printf.sprintf "unknown binary operator: %s" token
    ;;
  end

  let binop token a b =
    let open Term in
    match a, b with
    | Int a, Int b -> Int (token_to_arith token a b)
    | other, other' ->
      failwith
      @@ Printf.sprintf
           "cannot be performed %s with %s and %s"
           token
           (show other)
           (show other')
  ;;

  let rec eval1 = function
    | (Int _ | Fun _ | Nil) as i -> i
    | Var x | Label x -> Store.get x |> Value.to_term
    | App (Fun (x, body), e) ->
      let () = Store.set x (Value.from_term e) in
      body
    (* binary-operation form: l token r = ((token l) r) *)
    | App (App (PrimOp token, l), r) -> binop token l r
    | Let (x, bnd, body) ->
      let () = Store.set x (Value.from_term bnd) in
      body
    | Create f ->
      let l = genlabel () in
      let () = Store.set l @@ Value.from_term f in
      Label l
    | Resume (l, e) ->
      (match Value.from_term l with
      | Value.Label l ->
        let f = Store.get l |> Value.to_term in
        let () = Store.flush l in
        Builder.(labelE l (f <@> e))
      | _ -> failwith @@ Printf.sprintf "%s is not a label" @@ show l)
    | LabelE (l, e) ->
      (* C[l:e] ~~> C[l:C'[e']] *)
      let e', c = punch e in
      (match e' with
      | Yield v (* v can be a value *) ->
        let x = gensym () in
        let () = Store.set l @@ Value.Fun (x, c @@ Builder.var x) in
        v
      | v when is_value v -> v
      | other -> LabelE (l, c @@ eval1 other))
    | Yield _ -> failwith "yield from top-level is not allowed"
    | (App _ | PrimOp _) as inv ->
      failwith @@ Printf.sprintf "invalid term %s" @@ show inv
  ;;

  let rec eval t =
    if is_value t
    then t |> Value.from_term
    else (
      let t', ctx = punch t in
      eval1 t' |> ctx |> eval)
  ;;
end
