open! Core
open Env_intf

module type S = S

module Make (Id : Unique_id.Id) : S with type Id.t := Id.t
