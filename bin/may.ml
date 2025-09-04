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

module Path_shortener = struct
  module Short_path = struct
    module T = struct
      type t =
        { name : string
        ; id : int
        }
      [@@deriving hash, compare, equal, sexp_of]
    end

    include T
    include Hashable.Make_plain (T)

    let to_ident { name; id } = May.Ast.Ident.of_string [%string "<%{name}#%{id#Int}>"]

    let of_ident ident =
      let s = May.Ast.Ident.to_string ident in
      let s_sub = String.slice s 1 (-1) in
      let name, id_str = String.rsplit2_exn s_sub ~on:'#' in
      { name; id = Int.of_string id_str }
    ;;
  end

  type t =
    { to_full_path : string Short_path.Table.t
    ; to_short_path : Short_path.t String.Table.t
    ; short_counts : int String.Table.t
    }

  let create () =
    { to_full_path = Short_path.Table.create ()
    ; to_short_path = String.Table.create ()
    ; short_counts = String.Table.create ()
    }
  ;;

  let full_path_to_short_path t filepath =
    match Hashtbl.find t.to_short_path filepath with
    | Some short_path -> short_path
    | None ->
      let basename = Filename.basename filepath in
      let count = Hashtbl.find t.short_counts basename |> Option.value ~default:0 in
      let short_path = Short_path.{ name = basename; id = count } in
      Hashtbl.add_exn t.to_short_path ~key:filepath ~data:short_path;
      Hashtbl.add_exn t.to_full_path ~data:filepath ~key:short_path;
      Hashtbl.set t.short_counts ~key:basename ~data:(count + 1);
      short_path
  ;;
end

let make_load_file ~starting_file ~starting_ast () =
  let memo = Path_shortener.Short_path.Table.create () in
  let path_shortener = Path_shortener.create () in
  let short_starting_file =
    Path_shortener.full_path_to_short_path path_shortener starting_file
  in
  Hashtbl.add_exn memo ~key:short_starting_file ~data:starting_ast;
  ( Path_shortener.Short_path.to_ident short_starting_file
  , fun ~range ~current_file ~filepath ->
      let current_realpath =
        current_file
        |> Path_shortener.Short_path.of_ident
        |> Hashtbl.find_exn path_shortener.to_full_path
      in
      let dirname = Filename.dirname current_realpath in
      let new_file_path =
        match Filename.is_absolute filepath with
        | false -> Filename.concat dirname filepath
        | true -> filepath
      in
      let%bind.Result () =
        Unix.access new_file_path [ `Read ]
        |> Result.map_error ~f:(fun (_ : exn) ->
          May.Comp_error.create
            ~stage:May.Comp_error.Stage.Type_checker
            ~msg:[ Text "Could not open file "; Text filepath ]
            ~range)
      in
      let new_file_realpath = Filename_unix.realpath new_file_path in
      let new_file_shortpath =
        Path_shortener.full_path_to_short_path path_shortener new_file_realpath
      in
      let%bind.Result ast =
        match Hashtbl.find memo new_file_shortpath with
        | None ->
          let%bind.Result ast =
            new_file_realpath |> In_channel.read_all |> May.Parser.parse_string
          in
          Hashtbl.add_exn memo ~key:new_file_shortpath ~data:ast;
          Ok ast
        | Some ast -> Ok ast
      in
      Ok (Path_shortener.Short_path.to_ident new_file_shortpath, ast) )
;;

let compile_file ~mode ?out_file file_name =
  let out_file = Option.value_or_thunk out_file ~default:(fun () -> file_name ^ ".qbe") in
  let check = May.Check.empty ~mode in
  let file_name_realpath = Filename_unix.realpath file_name in
  let infile = In_channel.read_all file_name in
  let%bind.Result ast = May.Parser.parse_string infile in
  let starting_file, load_file =
    make_load_file ~starting_file:file_name_realpath ~starting_ast:ast ()
  in
  let%bind.Result checked_ast =
    May.Check.check_decls check ~decls:ast ~starting_file ~load_file
  in
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
       | Error e ->
         print_endline (May.Comp_error.to_string e);
         exit 1)
;;

let () = Command_unix.run command
