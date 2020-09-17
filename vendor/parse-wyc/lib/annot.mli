module type Annotated = sig
  type t

  val mk : unit -> t

  val is_generated : t -> bool
end

open Migrate_ast.Parsetree

module Exp : Annotated with type t = expression

module Attr : Annotated with type t = attribute

module Class_exp : Annotated with type t = class_expr

module Mod_expr : Annotated with type t = module_expr

module Mod_binding : Annotated with type t = module_binding

module Core_type : Annotated with type t = core_type
