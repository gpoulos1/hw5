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

  (* InferExpIdType looks through the environment to see if the id
   * has been declared and returns the type if so, otherwise
   * raises exception.
   *)
  fun inferExpIdType (g : env, id : Ast.id ) : AnnAst.typ =
    case g of 
         []   => raise UndeclaredError(id)
    | x::xs   => case M.find(x, id) of
                    NONE   => inferExpIdType(xs, id)
                  | SOME typ   => typ

  fun inferExp (e : Ast.exp, g : env ) : AnnAst.exp =
      case e of
        Ast.EInt(i)       => AnnAst.EInt(i)
      | Ast.EDouble(d)    => AnnAst.EDouble(d)
      | Ast.EString(s)    => AnnAst.EString(s)
      | Ast.ETrue         => AnnAst.ETrue
      | Ast.EFalse        => AnnAst.EFalse
      | Ast.EId(id)       => let
                                val typOfEId = inferExpIdType(g, id)
                              in
                                AnnAst.EId(id, typOfEId)
                              end
      (*
      | Ast.ECall(id, l)  => *)
      | Ast.EPostIncr(id) => (case inferExpIdType(g, id) of
                                  AnnAst.Tint => AnnAst.EPostDecr(id, inferExpIdType(g, id))
                                | _           => raise TypeError )
      | Ast.EPostDecr(id) => (case inferExpIdType(g, id) of
                                  AnnAst.Tint => AnnAst.EPostDecr(id, inferExpIdType(g, id))
                                | _           => raise TypeError )
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





