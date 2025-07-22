open! Core
module Unix = Core_unix

let main_of_entry_point entry_point =
  [%string
    "export\n\
    \ function w $main()\n\
     {\n\
     @start\n\
    \  %exit_code =w call $%{entry_point}()\n\
    \  ret %exit_code\n\
     }"]
;;

let compile_file ~mode ?out_file file_name =
  let out_file = Option.value_or_thunk out_file ~default:(fun () -> file_name ^ ".qbe") in
  let infile = In_channel.read_all file_name in
  let check = May.Check.empty ~mode in
  let%bind.Result ast = infile |> May.For_testing.parse_string in
  let%bind.Result checked_ast = May.Check.check_decls check ~decls:ast in
  let compiled_program, entry_point =
    May.Qbe_backend.Compilation_unit.compile_program ~check ~decls:checked_ast
  in
  let compiled_program_bytes =
    May.Qbe_backend.Compilation_unit.to_bytes compiled_program
  in
  Out_channel.with_file out_file ~f:(fun chan ->
    Out_channel.output_bytes chan compiled_program_bytes;
    Option.iter entry_point ~f:(fun ep ->
      Out_channel.output_string chan (main_of_entry_point ep)))
  |> Result.return
;;

let command =
  Command.basic
    ~summary:"compile to qbe"
    (let%map_open.Command () = return ()
     and file_name = anon ("filename" %: Filename_unix.arg_type)
     and out_file =
       flag
         "out"
         (optional Filename_unix.arg_type)
         ~doc:"OUT file to save compiler output to"
     and ownership =
       flag "ownership" no_arg ~doc:"OWNERSHIP enable ownership types in the compiler"
     in
     fun () ->
       let mode = if ownership then May.Mode.With_ownership else May.Mode.Without in
       match compile_file ~mode ?out_file file_name with
       | Ok () -> ()
       | Error e -> print_endline (May.Comp_error.to_string e))
;;

let () = Command_unix.run command
