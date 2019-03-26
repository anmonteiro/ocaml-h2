open Parsetree
open Ast_helper
open Asttypes

let strloc txt = { txt; loc = !default_loc }
