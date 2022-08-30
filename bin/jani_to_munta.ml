open Munta
open Symta.JANI
open Ppx_yojson_conv_lib
open Base
module Format = Caml.Format

(* 

(** Extend Z with support for `ppx_deriving` and `ppx_show`. *)
module Z = struct
  include Z

  let t_of_yojson = function
  | `Int i -> Z.of_int i
  | `Intlit s -> Z.of_string s
  | s -> raise (Yojson_conv.of_yojson_error "Z.t_of_yojson: invalid argument" s)

  let yojson_of_t x: Yojson.Safe.t = `Int (Z.to_int x)

  let show = Z.to_string

  let pp ppf i = Caml.Format.pp_print_string ppf (show i)

end

(** Extend the exported Munta code with some utilities. *)
module MC = struct

  include Model_Checker

  let of_int i = Int_of_integer (Z.of_int i)

  let to_int i = Z.to_int (integer_of_int i)

  let nat_of_int i = nat_of_integer (Z.of_int i)

  let int_of_nat i = Z.to_int (integer_of_nat i)

  let nat_of_yojson x = nat_of_int (int_of_yojson x)
  let yojson_of_nat x = yojson_of_int (int_of_nat x)


  (** Copy the JANI "record" definitions to derive utility functions.
     Unforunately, I didn't get `ppx_import` to work to reduce the boilerplate.
  *)

  type int = Model_Checker.int = Int_of_integer of Z.t
  [@@deriving yojson]

  type 'a bounded_type_ext = 'a Model_Checker.bounded_type_ext
    = Bounded_type_ext of int * int * 'a
    [@@deriving yojson]

  type typea = Model_Checker.typea = TBounded of unit bounded_type_ext | TClock
    [@@deriving yojson]
  type ('a, 'b) bexp = ('a, 'b)  Model_Checker.bexp =
    True | Not of ('a, 'b) bexp |
    And of ('a, 'b) bexp * ('a, 'b) bexp | Or of ('a, 'b) bexp * ('a, 'b) bexp |
    Imply of ('a, 'b) bexp * ('a, 'b) bexp | Eq of ('a, 'b) exp * ('a, 'b) exp |
    Lea of ('a, 'b) exp * ('a, 'b) exp | Lta of ('a, 'b) exp * ('a, 'b) exp |
    Ge of ('a, 'b) exp * ('a, 'b) exp | Gt of ('a, 'b) exp * ('a, 'b) exp
  and ('a, 'b) exp = ('a, 'b)  Model_Checker.exp = Const of 'b | Var of 'a |
    If_then_else of ('a, 'b) bexp * ('a, 'b) exp * ('a, 'b) exp |
    Binop of ('b -> 'b -> 'b) * ('a, 'b) exp * ('a, 'b) exp |
    Unop of ('b -> 'b) * ('a, 'b) exp
    [@@deriving yojson]
  type 'a assignment_ext = 'a Model_Checker.assignment_ext =
    Assignment_ext of string * (string, int) exp * nat * string option * 'a
    [@@deriving yojson]
  type 'a destination_ext = 'a Model_Checker.destination_ext =
    Destination_ext of
      string * unit option * unit assignment_ext list * string option * 'a
    [@@deriving yojson]
  type 'a edge_ext = 'a Model_Checker.edge_ext =
    Edge_ext of
      string * string option * unit option * (string, int) bexp *
        unit destination_ext list * string option * 'a
    [@@deriving yojson]
  type 'a sync_ext = 'a Model_Checker.sync_ext =
    Sync_ext of (string option) list * string option * string option * 'a
    [@@deriving yojson]
  type 'a variable_declaration_ext = 'a Model_Checker.variable_declaration_ext =
    Variable_declaration_ext of
      string * typea * bool * (string, int) exp option * 'a
    [@@deriving yojson]
  type 'a element_ext = 'a Model_Checker.element_ext =
    Element_ext of string * string list * string option * 'a
    [@@deriving yojson]
  type 'a composition_ext = 'a Model_Checker.composition_ext =
    Composition_ext of
      unit element_ext list * unit sync_ext list * string option * 'a
    [@@deriving yojson]
  type 'a transient_value_ext = 'a Model_Checker.transient_value_ext =
    Transient_value_ext of string * (string, int) exp * string option * 'a
    [@@deriving yojson]
  type 'a location_ext = 'a Model_Checker.location_ext =
    Location_ext of
      string * (string, int) bexp option * unit transient_value_ext list * 'a
    [@@deriving yojson]
  type 'a automaton_ext = 'a Model_Checker.automaton_ext =
    Automaton_ext of
      string * unit variable_declaration_ext list * unit option *
        unit location_ext list * string list * unit edge_ext list *
        string option * 'a
    [@@deriving yojson]
  type 'a action_ext = 'a Model_Checker.action_ext =
    Action_ext of string * string option * 'a
    [@@deriving yojson]
  type 'a model_ext = 'a Model_Checker.model_ext =
    Model_ext of
      int * string * unit * unit * unit option * unit action_ext list *
        unit list * unit variable_declaration_ext list * unit option * unit *
        unit automaton_ext list * unit composition_ext * 'a
    [@@deriving yojson]
  type 'a result = 'a Model_Checker.result =
    Result of 'a | Error of string list
    [@@deriving yojson]
  type ('a, 'b, 'c, 'd) sexp = ('a, 'b, 'c, 'd) Model_Checker.sexp =
    Truea | Nota of ('a, 'b, 'c, 'd) sexp |
    Anda of ('a, 'b, 'c, 'd) sexp * ('a, 'b, 'c, 'd) sexp |
    Ora of ('a, 'b, 'c, 'd) sexp * ('a, 'b, 'c, 'd) sexp |
    Implya of ('a, 'b, 'c, 'd) sexp * ('a, 'b, 'c, 'd) sexp | Eqa of 'c * 'd |
    Leb of 'c * 'd | Ltb of 'c * 'd | Gea of 'c * 'd | Gta of 'c * 'd |
    Loc of 'a * 'b
    [@@deriving yojson]
  type ('a, 'b, 'c, 'd) formula = ('a, 'b, 'c, 'd) Model_Checker.formula =
    EX of ('a, 'b, 'c, 'd) sexp |
    EG of ('a, 'b, 'c, 'd) sexp | AX of ('a, 'b, 'c, 'd) sexp |
    AG of ('a, 'b, 'c, 'd) sexp |
    Leadsto of ('a, 'b, 'c, 'd) sexp * ('a, 'b, 'c, 'd) sexp
    [@@deriving yojson]

  let yojson_of_unit (): Yojson.Safe.t = `Tuple []

  let show_model model =
    yojson_of_model_ext yojson_of_unit model
    |> Yojson.Safe.pretty_to_string

  let yojson_of_formula' =
    yojson_of_formula
      yojson_of_nat yojson_of_string yojson_of_string yojson_of_int
  
  let show_formula formula =
    yojson_of_formula' formula |> Yojson.Safe.pretty_to_string

end *)

(** Convert JANI to Munta's internal representation.

  The conversion contains two constructions:
  - Conversion of Boolean "indicator" variables (only assgined to constants
    true/false) to integer variables.
  - Conversion of local variables to global variables.
    Currently only supported for Boolean indicator variables.

  TODO: Separate these constructions from the conversion.
*)

exception Conversion_error of string

let string (s: string) = s
let identifier = string
let lvalue = string

let constant_value = function
| Int i -> `E (MC.Const (MC.of_int i))
| Bool true  -> `B MC.True
| Bool false -> `B (MC.Not MC.True)
| _ -> raise (Invalid_argument "Unsupported type")

let string_of_expression e =
  yojson_of_expression e |> Yojson.to_string

let ill_typed_exn e =
  let s = string_of_expression e in
  raise (Conversion_error (Printf.sprintf "Ill-typed expression: %s" s))

let global_name_of name x = name ^ "." ^ x

let local_to_global name = let open MC in function
(* TODO: This assumes that the only local variables are Boolean indicators. *)
| `E (Var x) -> `B (Eq (Var (global_name_of name x), Const (MC.of_int 1)))
| `E _ | `B _ ->
  Conversion_error
    "Conversion from local to global variables is not fully implemented!"
  |> raise

let rec expression = function
| Var x -> `E (MC.Var (identifier x))
| Const v -> constant_value v
| Binary {
    op : string;
    left: expression;
    right: expression;
  } as orig ->
    let left = expression left in
    let right = expression right in
    let mk f =
      match left, right with
      | `E l, `E r -> `E (MC.Binop (f, l, r))
      | _ -> ill_typed_exn orig
    in
    let mkb f =
      match left, right with
      | `B l, `B r -> `B (f l r)
      | _ -> ill_typed_exn orig
    in
    let mke f =
      match left, right with
      | `E l, `E r -> `B (f l r)
      | _ -> ill_typed_exn orig
    in
    begin match op with
    (* TODO: some of these are not part of JANI,
       and some of JANI are missing. *)
    | "*" -> mk MC.op_mul_int
    | "/" -> mk MC.op_div_int
    | "+" -> mk MC.op_plus_int
    | "-" -> mk MC.op_minus_int
    | "=" -> mke (fun l r -> MC.Eq (l, r))
    | "≠" -> mke (fun l r -> MC.Not (MC.Eq (l, r)))
    | "≤" -> mke (fun l r -> MC.Lea (l, r))
    | "≥" -> mke (fun l r -> MC.Ge (l, r))
    | ">" -> mke (fun l r -> MC.Gt (l, r))
    | "<" -> mke (fun l r -> MC.Lta (l, r))
    | "∧" -> mkb (fun l r -> MC.And (l, r))
    | "∨" -> mkb (fun l r -> MC.Or (l, r))
    | "→" -> mkb (fun l r -> MC.Imply (l, r))
    | s ->
      raise (Conversion_error (Printf.sprintf "Unsupported operator: %s" s))
    end
| Unary {
    op : string;
    exp: expression;
  } as orig ->
    let exp = expression exp in
    let mke f =
      match exp with
      | `E exp -> `E (MC.Unop (f, exp))
      | _ -> ill_typed_exn orig
    in
    let mkb f =
      match exp with
      | `B exp -> `B (f exp)
      | _ -> ill_typed_exn orig
    in
    begin match op with
    | "-" -> mke MC.op_neg_int (* TODO: is this part of JANI? *)
    | "¬" -> mkb (fun b -> MC.Not b)
    | _ -> ill_typed_exn orig
    end
| Local {
    name : identifier;
    exp : expression;
  } -> 
    local_to_global name (expression exp)

let expected_type_exn s e =
  string_of_expression e
  |> Printf.sprintf "Expected %s expression: %s" s
  |> fun x -> Conversion_error x
  |> raise

let exp e =
  match expression e with
  | `E e -> e
  | _ -> expected_type_exn "value" e

let bexp e =
  match expression e with
  | `B e -> e
  | _ -> expected_type_exn "Boolean" e

let element {
  automaton : string;
  input_enable : identifier list;
  comment : string option;
} = MC.Element_ext (
  automaton,
  List.map ~f:identifier input_enable,
  Option.map ~f:string comment,
  ()
)

let sync {
  synchronise : identifier option list;
  result : identifier option;
  comment : string option;
} = MC.Sync_ext (
  List.map ~f:(Option.map ~f:identifier) synchronise,
  Option.map ~f:identifier result,
  Option.map ~f:string comment,
  ()
)

let composition {
  elements : element list;
  syncs : sync list;
  comment : string option;
} = MC.Composition_ext (
  List.map ~f:element elements,
  List.map ~f:sync syncs,
  Option.map ~f:string comment,
  ()
)

let bounded_type {
  lower_bound : int;
  upper_bound : int;
} = MC.Bounded_type_ext (
  MC.of_int lower_bound,
  MC.of_int upper_bound,
  ()
)

let conv_typ = function
| TBounded bt -> MC.TBounded (bounded_type bt)
| TClock -> MC.TClock
| TBool -> raise (Invalid_argument "Boolean type is not supported")

let cast_exp ref e =
  match expression e with
  | `E e -> e
  | `B MC.True -> MC.Const (MC.of_int 1)
  | `B MC.(Not True) -> MC.Const (MC.of_int 0)
  | `B _ ->
    ref
    |> Printf.sprintf "Boolean variables are only supported as indicators: %s"
    |> fun x -> Conversion_error x
    |> raise

let variable_declaration ?(automaton=None) {
  name : identifier;
  typ: typ;
  transient : bool;
  initial_value : expression option;
} =
  let typ, conv_value, name =
  match typ, automaton with
  | TBool, Some automaton ->
    Stdio.printf
      "Warning: Converting Boolean variable '%s' to integer (unverified)\n"
      name;
    TBounded {lower_bound = 0; upper_bound = 1},
    cast_exp name,
    global_name_of automaton name
  | t, None -> t, exp, name
  | _ ->
    raise (Conversion_error "Only Boolean variables can be globalized!")
  in
  MC.Variable_declaration_ext (
    identifier name,
    conv_typ typ,
    transient,
    Option.map ~f:conv_value initial_value,
    ()
  )

let action ({
  name : identifier;
  comment : string option;
}: action) = MC.Action_ext (
  identifier name,
  Option.map ~f:string comment,
  ()
)

let transient_value ({
  ref : lvalue;
  value : expression;
  comment : string option;
}: transient_value) = MC.Transient_value_ext (
  lvalue ref,
  exp value,
  Option.map ~f:string comment,
  ()
)

let commented_expression ({
  exp : expression = e;
  _
}: commented_expression) = exp e

let location {
  name : identifier;
  time_progress :
    commented_expression = {
      exp : expression = e;
      _
    };
  transient_values :
    transient_value list;
  comment : string option = _comment;
} = MC.Location_ext (
  identifier name,
  Some (bexp e),
  List.map ~f:transient_value transient_values,
  ()
)

let assignment local_var_names automaton {
  ref : lvalue;
  value : expression;
  index : int;
  comment : string option;
} =
  MC.Assignment_ext (
    (if List.mem ~equal:String.equal local_var_names ref then
      global_name_of automaton (lvalue ref)
    else lvalue ref),
    cast_exp ref value,
    MC.nat_of_int index,
    Option.map ~f:string comment,
    ()
  )

let destination local_var_names automaton {
  location : identifier;
  probability :
    commented_expression = {
      exp : expression = e;
      _
    };
  assignments : assignment list;
  comment : string option;
} =
  match e with
  | Const (Int 1) -> MC.Destination_ext (
    identifier location,
    None,
    List.map ~f:(assignment local_var_names automaton) assignments,
    Option.map ~f:string comment,
    ()
  )
  | _ -> raise (Invalid_argument "Destination probability must be 1")

let edge local_var_names automaton {
  location : identifier;
  action : identifier option;
  (* rate : unit option; *)
  guard : commented_expression = {
    exp : expression = e;
    _
  };
  destinations : destination list;
  comment : string option;
} = MC.Edge_ext (
  identifier location,
  Option.map ~f:identifier action,
  None,
  bexp e,
  List.map ~f:(destination local_var_names automaton) destinations,
  Option.map ~f:string comment,
  ()
)

let automaton {
  name : identifier;
  variables : variable_declaration list;
  (* restrict_initial : unit option; *)
  locations : location list;
  initial_locations : identifier list;
  edges : edge list;
  comment : string option;
} =
(
  let var_decls =
    List.map ~f:(variable_declaration ~automaton:(Some name)) variables in
  let local_var_names = List.map variables
    ~f:(fun (decl: variable_declaration) -> decl.name) in
  MC.Automaton_ext (
    identifier name,
    [],
    None,
    List.map ~f:location locations,
    List.map ~f:identifier initial_locations,
    List.map ~f:(edge local_var_names name) edges,
    Option.map ~f:string comment,
    ()
  ),
  var_decls
)

let ensure_cond_pair = let open MC in function
| Var x, Const i -> (x, i)
| _ -> raise (Conversion_error "Expected variable and constant")

let rec sexp_of_bexp = let open MC in function
| True -> Truea
| Not b -> Nota (sexp_of_bexp b)
| And (b1, b2) -> Anda (sexp_of_bexp b1, sexp_of_bexp b2)
| Or (b1, b2) -> Ora (sexp_of_bexp b1, sexp_of_bexp b2)
| Imply (b1, b2) -> Implya (sexp_of_bexp b1, sexp_of_bexp b2)
| Eq (x, i) -> let x, i = ensure_cond_pair (x, i) in Eqa (x, i)
| Lta (x, i) -> let x, i = ensure_cond_pair (x, i) in Ltb (x, i)
| Lea (x, i) -> let x, i = ensure_cond_pair (x, i) in Leb (x, i)
| Ge (x, i) -> let x, i = ensure_cond_pair (x, i) in Gea (x, i)
| Gt (x, i) -> let x, i = ensure_cond_pair (x, i) in Gta (x, i)

let sexp (e: expression) =
  bexp e |> sexp_of_bexp

let property_expression {
  op: ctl_operator;
  exp: expression = e;
} = match op with
  | EF -> MC.EX (
    sexp e
  )

let property {
  name: identifier = _name;
  expression: property_expression = prop;
  comment: string option = _comment;
} = property_expression prop

let model {
  jani_version : int;
  name : string;
  (* metadata : unit; *)
  typ : string = _typ;
  (* features : unit option; *)
  actions : action list;
  (* constants : unit list; Kill option for convenience *)
  variables : variable_declaration list;
  (* restrict_initial : unit option; *)
  automata : automaton list;
  system : composition;
  properties : property list = _properties;
} =
  let var_decls, automata = List.fold_map automata ~init:[] ~f:(
    fun acc a -> let a, decls = automaton a in decls :: acc, a)
  in
  let var_decls = List.rev var_decls |> List.concat in
  let all_decls =
    List.map ~f:variable_declaration variables @ var_decls in
  let () = Stdio.print_endline "hi" in
  let () =
    List.iter all_decls ~f:(function
    | Variable_declaration_ext (name, _, _, _, _) ->
      Stdio.print_endline name
    ) in
  MC.Model_ext (
    MC.of_int jani_version,
    identifier name,
    (),
    (),
    None,
    List.map ~f:action actions,
    [],
    List.map ~f:variable_declaration variables @ var_decls,
    None,
    (), (* List.map ~f:property properties, *)
    automata,
    composition system,
    ()
  )