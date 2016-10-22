(*  COMP 321 Homework 5:  CPP typing engine.
*   
*   N. Danner
*)

structure Typing =
struct

      (* the ORD_KEY signature to be used in SplayMapFn *)
      structure K =
      struct

        type ord_key = Ast.id
        val compare = String.compare

      end

      (* Our SplatMapFn structure *)
      structure M = SplayMapFn(K) 

      (* Our environment type *)
      type env = (AnnAst.typ M.map) list
 
  exception TypeError
  exception UndeclaredError of Ast.id
  exception MultiplyDeclaredError of Ast.id
  exception ReturnTypeError

  (* findIdTyp looks through the environment to see if the id
   * has been declared and returns the type if so, otherwise
   * raises exception.
   *)

  fun findIdTyp ( id : string, g : env ) : AnnAst.typ =
        case g of 
          []    => raise UndeclaredError(id)
        | x::xs => case M.find(x, id) of
                          NONE     => findIdTyp(id, xs)
                        | SOME typ => typ

  (* A helper function used for increment/decrement expressions.
   * Makes sure that the id has type int and returns the same type,
   * otherwise raises a type error.
   *)

  fun incrDecrHelp (id : string, g : env) : AnnAst.typ =
    case findIdTyp(id, g) of
      AnnAst.Tint => AnnAst.Tint
      | _         => raise TypeError

(* Takes in an AnnAst.exp literal and returns the corresponding
 * AnnAst.typ.
 *)

  fun inferExp (e : Ast.exp, g : env ) : AnnAst.exp =
      case e of
        Ast.EInt(i)       => AnnAst.EInt(i)
      | Ast.EDouble(d)    => AnnAst.EDouble(d)
      | Ast.EString(s)    => AnnAst.EString(s)
      | Ast.ETrue         => AnnAst.ETrue
      | Ast.EFalse        => AnnAst.EFalse

      (*
      | Ast.ECall(id, l)  => *)

    (* This looks at type of the id, and if it is a AnnAst.Tint, returns
     * the annotated version with the typ and id.  Otherwise it
     * raises a type error.
     *)
      | Ast.EPostIncr(id) => (case findIdTyp(id, g) of
                                  AnnAst.Tint => AnnAst.EPostDecr(id, findIdTyp(id, g))
                                | _           => raise TypeError )
      | Ast.EPostDecr(id) => (case findIdTyp(id, g) of
                                  AnnAst.Tint => AnnAst.EPostDecr(id, findIdTyp(id, g))
                                | _           => raise TypeError )

     (* This looks at the literal value of the expression
      * and if it is either a ETrue or EFalse, returns an
      * AnnAst.ENot() with the value of the expression and
      * the Tbool type.  Otherwise raises a type error.
      *)
      | Ast.ENot(exp)     => (case inferExp(exp, g) of
                                AnnAst.ETrue  => AnnAst.ENot(inferExp(exp, g), AnnAst.Tbool)
                              | AnnAst.EFalse => AnnAst.ENot(inferExp(exp,g), AnnAst.Tbool)
                              | _             => raise TypeError )
      | _                 => raise TypeError

  (*  inferExpNoEnv e = e', where e' is the annotated expression
  *   corresponding to e.  e must be typeable from the empty environment.
  *
  *   Really you need to define a function inferExp that takes an 
  *   environment and an expression, and just call that function with
  *   an empty environment.
  *)
  fun inferExpNoEnv (e : Ast.exp) : AnnAst.exp =
    inferExp(e, [])

  (*  checkPgm p = p', where p' is the annotated program corresponding to p'.
  *)
  fun checkPgm (p : Ast.program) : AnnAst.program =
    raise TypeError

end





