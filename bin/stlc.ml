(** Using the pretty printer to format a simply typed lambda calculus. *)

(**************************************************************************)
(** Instantiate the pretty-printer. *)

type annot = unit

module PpString = Ann_print.Pp (Ann_print.StringBackend (struct
  type t = annot
end))

(**************************************************************************)
(** Define the lambda calculus. *)

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

let paren doc =
  let open Ann_print in
  char '(' ^^ doc ^^ char ')'

let rec pp_type (ty : type_) : annot Ann_print.doc =
  let open Ann_print in
  match ty with
  | Atom v -> string v
  | Arrow (t1, t2) ->
      let pp1 = if is_arrow t1 then paren @@ pp_type t1 else pp_type t1 in
      let sep = space ^^ string "->" ^^ space in
      flow sep [ pp1; pp_type t2 ]

let rec pp_term (t : term) : annot Ann_print.doc =
  let open Ann_print in
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

let () =
  (* Test on a few terms. *)
  let print t = Format.printf "%s\n\n" (PpString.pp ~width:30 (pp_term t)) in
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
