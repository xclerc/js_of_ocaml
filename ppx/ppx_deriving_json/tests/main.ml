let _ = Js_of_ocaml_deriving_json.read
let () = Toplevel_expect_test.run (fun _ -> Ppx_deriving.mapper)
