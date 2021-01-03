module Syntax = Syntax

module Store : sig
  val reflesh : unit -> unit
end =
  Eval.Store

let eval = Eval.eval
