type t =
  | Nil
  | Int of int
  | Var of string
  | Let of string * t * t
  | Fun of string * t
  | App of t * t
  | PrimOp of string
  | Create of t
  | Resume of t * t
  | Yield of t
  | Label of string
  | LabelE of string * t
[@@deriving show { with_path = false }]

let is_value = function
  | Nil | Int _ | Fun _ | Label _ -> true
  | _ -> false
;;

module Builder = struct
  open struct
    let gensym = Utils.gengensym "x"
  end

  let nil = Nil
  let int v = Int v
  let var x = Var x

  let fn f : t =
    let x = gensym () in
    Fun (x, f @@ var x)
  ;;

  let ( let@ ) v k =
    let x = gensym () in
    Let (x, v, k @@ var x)
  ;;

  let ( <@> ) f a = App (f, a)
  let prim s = PrimOp s
  let ( <+> ) l r = prim "+" <@> l <@> r
  let ( <-> ) l r = prim "-" <@> l <@> r
  let create f = Create f
  let resume co a = Resume (co, a)
  let yield v = Yield v
  let label s = Label s
  let labelE l e = LabelE (l, e)
end
