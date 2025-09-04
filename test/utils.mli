open! Core

val remove_indentation : string -> string
val starting_file : May.Ast.Ident.t

val load_file
  :  mappings:(string, 'a) Base.List.Assoc.t
  -> range:May.Range.t
  -> current_file:May.Ast.Ident.t
  -> filepath:string
  -> (May.Ast.Ident.t * 'a, May.Comp_error.t) Result.t
