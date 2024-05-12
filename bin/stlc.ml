(** Using the pretty printer to format a simply typed lambda calculus. *)
open Ann_print

(**************************************************************************)
(** Instantiate the pretty-printer. *)

type annot = Backends.XmlBackend.annot

module PpString = Pp.Make (Backends.StringBackend (struct
  type t = annot
end))

module PpXml = Pp.Make (Backends.XmlBackend)

(**************************************************************************)
(** Define a lambda calculus. *)

(** A term variable (e.g. "x" or "y"). *)
type var = string

(** A type variable (e.g. "nat" or "bool"). *)
type tvar = string

(** The grammar of simple types. *)
type type_ = Atom of tvar | Arrow of type_ * type_

(** The grammar of simply-typed lambda terms. *)
type term =
  | Var of var
  | App of term * term
  | Abs of var * type_ * term
  | LetIn of var * type_ * term * term

let is_arrow (ty : type_) : bool = match ty with Arrow _ -> true | _ -> false
let is_var (t : term) : bool = match t with Var _ -> true | _ -> false
let is_app (t : term) : bool = match t with App _ -> true | _ -> false

(**************************************************************************)
(** Print the lambda calculus. *)

let paren doc =
  let open Pp in
  char '(' ^^ doc ^^ char ')'

let rec pp_type (ty : type_) : annot Pp.doc =
  let open Pp in
  match ty with
  | Atom v -> string v
  | Arrow (t1, t2) ->
      let pp1 = if is_arrow t1 then paren @@ pp_type t1 else pp_type t1 in
      let sep = space ^^ string "->" ^^ space in
      flow sep [ pp1; pp_type t2 ]

let rec pp_term (t : term) : annot Pp.doc =
  let open Pp in
  match t with
  | Var v -> string v
  | App (t1, t2) ->
      (if is_var t1 || is_app t1 then pp_term t1 else paren @@ pp_term t1)
      ^//^ if is_var t2 then pp_term t2 else paren @@ pp_term t2
  | Abs (x, ty, body) ->
      let binder = string "fun" ^+^ string x ^+^ char ':' in
      let type_ = pp_type ty ^+^ string "=>" in
      (binder ^//^ type_) ^//^ pp_term body
  | LetIn (x, ty, def, body) ->
      let binder = string "let" ^+^ string x ^+^ string ":" in
      let type_ = pp_type ty ^+^ string "=" in
      let body = string "in" ^/^ pp_term body in
      ((binder ^//^ type_) ^//^ pp_term def) ^/^ body

(**************************************************************************)
(** Test on a few terms. *)

let width = 30

let () =
  let print t =
    let open Tyxml in
    let output =
      Xml.node "span" ~a:[ Xml.string_attrib "style" "white-space: pre" ]
      @@ PpXml.pp ~width (pp_term t)
    in
    Format.printf "%s\n%a\n" (String.make width '-') (Xml.pp ()) output
  in
  print
  @@ Abs
       ( "f"
       , Arrow (Arrow (Atom "nat", Atom "nat"), Atom "bool")
       , App (Var "f", App (Var "g", Var "h")) );
  print
  @@ LetIn
       ( "f"
       , Arrow (Arrow (Atom "nat", Atom "nat"), Atom "bool")
       , App (Var "g", Var "x")
       , Abs ("f", Arrow (Arrow (Atom "nat", Atom "nat"), Atom "bool"), App (Var "f", Var "g")) );
  print
  @@ LetIn ("f", Atom "bool", App (Var "g", Var "x"), Abs ("f", Atom "bool", App (Var "f", Var "g")))
