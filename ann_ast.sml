(*  COMP 321 Homework 5:  Type-checking for a fragment of C.
*   
*   N. Danner.
*)

structure AnnAst =
struct

  (*  The type of identifiers.
  *)
  type id = string

  (*  You will need a type for the source language types.  Do not
  *   use the TNone constructor.
  *)
  datatype typ = Tbool | Tint | Tdouble | Tstring | Tvoid

  (*  The annotated type of expressions.  I have provided a couple of 
  *   clauses for you, but you need to supply the rest.
  *)
  datatype exp = EInt of int | EDouble of real | EString of string (*  Doesn't need a type parameter, because
                                                                       an EInt can only be of type int. *)
               | ETrue | EFalse 
               | EId of id*typ
               | ECall of id*(exp list)*typ
               | EPostIncr of id*typ | EPostDecr of id*typ
               | ENot of exp*typ
               | EPreIncr of id*typ | EPreDecr of id*typ
               | EMul of exp*exp*typ | EDiv of exp*exp*typ | EMod of exp*exp*typ
               | EAdd of exp*exp*typ | ESub of exp*exp*typ (*  Needs a type parameter, because a +
                                                       expression could have more than one type. *)
               | ELShift of exp*exp*typ | ERShift of exp*exp*typ
               | ELt of exp*exp*typ | EGt of exp*exp*typ 
               | ELe of exp*exp*typ | EGe of exp*exp*typ
               | EEq of exp*exp*typ | ENeq of exp*exp*typ 
               | EAnd of exp*exp*typ 
               | EOr of exp*exp*typ
               | EAsst of id*exp*typ
               | ECond of exp*exp*exp*typ


 (*  The type of statements.
  *)
  datatype stm = SExp of exp
               | SDecl of typ*(id list)
               | SInit of typ*((id*exp) list)
               | SReturn of exp 
               | SDoWhile of stm*exp
               | SWhile of exp*stm
               | SFor of (typ*id*exp)*exp*exp*stm
               | SBlock of stm list
               | SIf of exp*stm
               | SIfElse of exp*stm*stm

 (*  Parameters and prototypes.
  *)
  type paramdecl = typ*id
  type prototype = typ
              
  (*  The type of definitions.
  *)
  datatype def = DFun of typ*id*(paramdecl list)*(stm list)
               | DFunProt of typ*id*(prototype list)

  (*  The type of programs.
  *)
  datatype program = PDefs of def list

  (*  You must supply a function to convert (annotated) expressions to
  *   strings for the driver program.
  *)
  fun expToString (e : exp) : string =
    ""


  (*  You must spply a function to convert (annotated) programs to
  *   strings for the driver program.
  *)
  fun programToString(p : program) : string =
    ""

end
