(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type uop = Neg | Not

type fieldModifier = Static
type accControl = Public | Private | Protect

type typ = Int | Bool | Double| Void | String | IntList | BoolList | DoubleList | StringList
type controlFlow = Break | Continue 

type bind = typ * string

type expr =
    Literal of int
  | Dliteral of string
  | BoolLit of bool
  | StringLiteral of string
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | ObjField of string * string
  | ObjMethod of string * string * expr list
  | ObjDef of string * string
  | PreDefAsn of typ * string * expr option
  | ObjDefAsn of string * string * expr
  | Asn of string * expr
  | ObjAsn of string * expr
  | Call of string * expr list
  | ControlFlow of controlFlow
  | ListExpr of expr list option
  | Indexing of string * int
  | Noexpr

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type fundef = 
    {
      ty : typ;
      id : string;
      args : (typ * string) list;
      body :stmt list;
    }

type classStmt =  
    ConstructorDef of string * (typ * string)list * (string * expr) list
  | FieldDef of accControl option * fieldModifier option* typ * string * expr option
  | MethodDef of accControl option * fieldModifier option * fundef
      
  
type classdef = 
    {
     id : string;
     father: string option;
     interface: string list option;
     body: classStmt list;
     }

type absFunDef = 
      {
       fieldM : fieldModifier option;
       ty : typ;
       id : string;
       args : (typ * string) list;
       }

type interfaceDef =  
    { 
      id : string; 
      extend_members : string list option;
      body : absFunDef list;
    }

type programComp = 
    Stmt of stmt
  | Fun of fundef
  | Class of classdef
  | Interface of interfaceDef 

type program = programComp list

(* Unparser function *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_type = function
    Int -> "int"
  | Bool -> "bool"
  | Double -> "double"
  | Void -> "void"
  | String -> "string"
  | IntList -> "int[]"
  | DoubleList -> "double[]"
  | BoolList -> "bool[]"
  | StringList -> "string[]"

let rec string_of_expr = function
    Literal(l) -> string_of_int l
  | Dliteral(l) -> l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLiteral(l) -> l
  | Id(l) -> l
  | Binop(e1, o, e2) -> string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | ObjField(name, field) -> name ^ "." ^ field
  | ObjMethod(name, meth, el) -> name ^ "." ^ meth ^ "(" ^  String.concat ", " (List.map string_of_expr el) ^ ")"
  (* int a = 1 *)
  | PreDefAsn(t, n, e) -> 
    (match e with
      Some value -> string_of_type t ^ " " ^ n ^ " = " ^ string_of_expr value
    | None -> string_of_type t ^ " " ^ n)
  (* Dog a *)
  | ObjDef(t, n) -> t ^ " " ^ n
  (* Dog a =  new Dog() *)
  | ObjDefAsn(t, n, e) -> t ^ " " ^ n ^ " = new " ^ string_of_expr e
  | Asn(n, e) -> n ^ " = " ^ string_of_expr e
  | ObjAsn(n, e) -> n ^ " = " ^ string_of_expr e
  | Call(n, el) -> n ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | ControlFlow(Break) -> "break"
  | ControlFlow(Continue) -> "continue"
  | ListExpr(el) ->
    (match el with Some lis -> "[" ^ String.concat ", " (List.map string_of_expr lis) ^ "]"
    | None -> "[]")
  | Indexing(id, idx) -> id ^ "[" ^ string_of_int idx ^ "]"
  | Noexpr -> ""


let rec string_of_stmt = function
    Block(stmts) ->
      "{\n    " ^ String.concat "\n    " (List.map string_of_stmt stmts) ^ "\n}"
  | Expr(expr) -> string_of_expr expr ^ ";";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ " else " ^ string_of_stmt s2 ^ "\n    "
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s


let string_of_fundef (fd : fundef)= 
  string_of_type fd.ty ^ " " ^ fd.id ^ 
  "(" ^ String.concat ", " (List.map (function (t, s) -> string_of_type t ^ " " ^ s) fd.args) 
  ^ ") {\n    " ^ 
  String.concat "\n    " (List.map string_of_stmt fd.body) ^ "\n}\n"

let string_of_ac = function
    Some Public -> "public"
  | Some Private -> "private"
  | Some Protect -> "protect"
  | None -> "public"

let string_of_fm = function
  | Some Static -> "static"
  | None -> ""

let string_of_classStmt = function
      ConstructorDef(s, bind1, bind2) -> s ^ "(" ^
      (String.concat ", " (List.map (function (t, s) -> string_of_type t ^ " " ^ s) bind1)) ^ ") {\n    " ^
      (String.concat ";\n    " (List.map (function (s, e) -> "this." ^ s ^ " = " ^ string_of_expr e) bind2)) ^ "\n}"
    | FieldDef(ac, fm, t, s, e) -> 
      (match e with
      | Some exp -> string_of_ac ac ^ " " ^ string_of_fm fm ^ " " ^ string_of_type t ^ " " ^ s ^ " = " ^ string_of_expr exp ^ ";"
      | None -> string_of_ac ac ^ " " ^ string_of_fm fm ^ " " ^ string_of_type t ^ " " ^ s ^ ";")
    
    | MethodDef(ac, fm, fd) -> string_of_ac ac ^ " " ^ string_of_fm fm ^ " " ^ string_of_fundef fd

let string_of_father = function
    Some value -> "extends " ^ value
  | None -> ""

let string_of_abs_interface = function
    Some value -> "extends " ^ (String.concat ", " value)
  | None -> ""

let string_of_class_interface = function
  Some value -> "implements " ^ (String.concat ", " value)
| None -> ""

let string_of_classdef (cd : classdef) = 
  "class " ^ cd.id ^ " " ^ string_of_father cd.father ^ " " ^ string_of_class_interface cd.interface ^ " {\n    " ^
  String.concat "\n    " (List.map string_of_classStmt cd.body) ^ "\n}\n"

let string_of_absfundef absfundef = 
  string_of_fm absfundef.fieldM ^ " " ^ string_of_type absfundef.ty ^ " " ^ absfundef.id ^ "(" ^
  (String.concat ";\n" (List.map (function (t, s) -> string_of_type t ^ " " ^ s) absfundef.args)) ^ ");"
  
let string_of_interfacedef interfacedef = 
  "interface " ^ interfacedef.id ^ " " ^ string_of_abs_interface interfacedef.extend_members ^ " {\n    " ^ 
  (String.concat "\n   " (List.map string_of_absfundef interfacedef.body)) ^ "\n}"

let string_of_programcomp = function
    Stmt(s) -> string_of_stmt s
  | Fun(f) -> string_of_fundef f
  | Class(c) -> string_of_classdef c
  | Interface(i) -> string_of_interfacedef i

let string_of_program program = 
  String.concat "\n" (List.map string_of_programcomp program)