open Migrate_ast.Ast_helper
open Migrate_ast.Parsetree

module type Annotated = sig
  type t

  val mk : unit -> t

  val is_generated : t -> bool
end

module Ext = struct
  let mk () = (Location.mkloc "merlin.hole" !default_loc, PStr [])

  let is_generated = function
    | (({ txt = "merlin.hole"; _ }, PStr []) : extension) -> true
    | _ -> false
end

module Exp = struct
  type t = expression

  let mk () = Exp.extension (Ext.mk ())

  let is_generated e =
    match e.pexp_desc with
    | Pexp_extension ext when Ext.is_generated ext -> true
    | _ -> false
end

module Attr = struct
  type t = attribute

  let mk () = Attr.mk { txt = "merlin.hole.gen"; loc = Location.none } (PStr [])

  let is_generated a =
    match (a.attr_name.txt, a.attr_payload) with
    | "merlin.hole.gen", PStr [] -> true
    | _ -> false
end

module Class_exp = struct
  type t = class_expr

  let mk () = Cl.extension (Ext.mk ())

  let is_generated e =
    match e.pcl_desc with
    | Pcl_extension ext when Ext.is_generated ext -> true
    | _ -> false
end

module Mod_expr = struct
  type t = module_expr

  let mk () = Mod.extension (Ext.mk ())

  let is_generated e =
    match e.pmod_desc with
    | Pmod_extension ext when Ext.is_generated ext -> true
    | _ -> false
end

module Mod_binding = struct
  type t = module_binding

  let mk () = Mb.mk {txt= ""; loc= Location.none} (Mod_expr.mk ())

  let is_generated e = Mod_expr.is_generated e.pmb_expr
end

module Core_type = struct
  type t = core_type

  let mk () = Typ.extension (Ext.mk ())

  let is_generated t =
    match t.ptyp_desc with
    | Ptyp_extension ext when Ext.is_generated ext -> true
    | _ -> false
end

module Mod_type = struct
  type t = module_type

  let mk () = Mty.extension (Ext.mk ())

  let is_generated t =
    match t.pmty_desc with
    | Pmty_extension ext when Ext.is_generated ext -> true
    | _ -> false
end

module Mod_type_decl = struct
  type t = module_type_declaration

  let mk () = Mtd.mk {txt= ""; loc= Location.none} ~typ:(Mod_type.mk ())

  let is_generated t =
    match t.pmtd_type with
    | Some ty when Mod_type.is_generated ty -> true
    | _ -> false
end
