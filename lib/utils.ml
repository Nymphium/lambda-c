let gengensym prefix =
  let x = ref 0 in
  fun () ->
    let () = incr x in
    Printf.sprintf "%s%d" prefix !x
;;
