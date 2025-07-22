open! Core

module Local = struct
  module T = struct
    type t =
      { name : Ast.Ident.t [@compare.ignore] [@hash.ignore]
      ; id : int
      }
    [@@deriving sexp_of, fields ~getters, hash, compare]
  end

  include T
  include Hashable.Make_plain (T)
  include Comparable.Make_plain (T)
  module Table = Hashtbl.Make_plain (T)

  let create ~name ~id = { name; id }
  let to_string { name; id = _ } = Ast.Ident.to_string name

  let to_mangled_string { name; id } =
    "l_" ^ Ast.Ident.to_string name ^ "_" ^ Int.to_string id
  ;;
end

module Global = struct
  module Id = Unique_id.Int63 ()

  type t =
    { fq_name : string Nonempty_list.t
      (** A fully qualified name is the list of modules up to the root that 
          contain the definition. This is stored with the name as the first
          element of the list and surrounding modules are stored afterwards
          
          E.g. [A.B.c] is stored as [['c'; 'B'; 'A']]*)
    ; id : Id.t
      (** An Id is uniquely assigned upon creation. Every global has a unique ID. *)
    }
  [@@deriving sexp_of, fields ~getters]

  let create ~scope ~name = { fq_name = name :: scope; id = Id.create () }

  let to_string { fq_name; id = _ } =
    fq_name |> Nonempty_list.to_list |> List.rev |> String.concat ~sep:"."
  ;;

  let to_mangled_string { fq_name; id } =
    "global__"
    ^ (fq_name |> Nonempty_list.to_list |> List.rev |> String.concat ~sep:"__")
    ^ "__"
    ^ Id.to_string id
  ;;

  let equal { id = id1; fq_name = _ } { id = id2; fq_name = _ } = Id.equal id1 id2

  let%expect_test "global id examples" =
    let a = create ~scope:[] ~name:"a" in
    print_endline (to_string a);
    [%expect {| a |}];
    print_endline (to_mangled_string a);
    [%expect {| global__a__0 |}];
    let b = create ~scope:[ "A"; "B" ] ~name:"c" in
    print_endline (to_string b);
    [%expect {| B.A.c |}];
    print_endline (to_mangled_string b);
    [%expect {| global__B__A__c__1 |}]
  ;;
end
