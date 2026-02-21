let () =
  let usage = "slobby - a proglang for lanzy people\nUsage: slobby <file.slb> --target perl|ocaml" in
  let target = ref "" in
  let filename = ref "" in
  let speclist = [
    ("--target", Arg.Set_string target, "Target language: perl or ocaml");
  ] in
  Arg.parse speclist (fun f -> filename := f) usage;
  if !filename = "" then (
    Printf.eprintf "%s\n" usage;
    exit 1
  );
  if !target = "" then (
    Printf.eprintf "Error: --target is required (perl or ocaml)\n";
    exit 1
  );
  let source = In_channel.with_open_text !filename In_channel.input_all in
  let ast = Slobby_lib.Slobby.parse source in
  match !target with
  | "perl" ->
    let code = Slobby_lib.Codegen_perl.generate ast in
    print_string code
  | "ocaml" ->
    let code = Slobby_lib.Codegen_ocaml.generate ast in
    print_string code
  | t ->
    Printf.eprintf "Unknown target: %s (use perl or ocaml)\n" t;
    exit 1
