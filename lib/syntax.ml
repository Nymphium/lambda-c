module Term = struct
  type t =
    | Int of int
    | Var of string
    | Let of string * t * t
    | Fun of string * t
    | App of t * t
    | PrimOp of string
    | Create of t
    | Resume of t * t
    | Yield of t
  [@@deriving show { with_path = false }]

  module Builder = struct
    open struct
      let gensym =
        let x = ref 0 in
        fun () ->
          let () = incr x in
          Printf.sprintf "x%d" !x
      ;;
    end

    let int v = Int v
    let var x = Var x

    let fn f : t =
      let x = var @@ gensym () in
      f x
    ;;

    let ( let@ ) v k =
      let x = gensym () in
      Let (x, v, k @@ var x)
    ;;

    let ( @ ) f a = App (f, a)
    let prim s = PrimOp s
    let create f = Create f
    let resume co a = Resume (co, a)
    let yield v = Yield v
  end
end
