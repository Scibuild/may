open! Core

module Row : sig
  type t
end

type t

val max_fields_size : t -> class_id:Type.Class_id.t -> int
val methods_size : t -> class_id:Type.Class_id.t -> int
val of_signatures : sizeof:(Type.t -> int) -> Type.Class.t Type.Class_id.Table.t -> t
val field_offset : t -> class_id:Type.Class_id.t -> field:Ast.Ident.t -> int
val method_offset : t -> class_id:Type.Class_id.t -> method_:Ast.Ident.t -> int
