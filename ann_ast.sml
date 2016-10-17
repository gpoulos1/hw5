(*  COMP 321 Homework 5:  Type-checking for a fragment of C.
*   
*   N. Danner.
*)

structure AnnAst =
struct

  (*  You will need a type for the source language types.  Do not
  *   use the TNone constructor.
  *)
  datatype typ = TNone

  (*  The annotated type of expressions.  I have provided a couple of 
  *   clauses for you, but you need to supply the rest.
  *)
  datatype exp = EInt of int (*  Doesn't need a type parameter, because
                                 an EInt can only be of type int. *)
               | EAdd of (exp*exp)*typ 
                             (*  Needs a type parameter, because a +
                                 expression could have more than one type. *)

  (*  The type of programs.  Replace the PNone constructor with your
  *   definition.
  *)
  datatype program = PNone

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
