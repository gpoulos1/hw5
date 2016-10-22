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

  (* ensureInt is a helper function used for increment/decrement expressions.
   * Makes sure that the id has type int and returns the same type,
   * otherwise raises a type error.
   *)
  fun ensureInt (id : string, g : env) : AnnAst.typ =
    case findIdTyp(id, g) of
      AnnAst.Tint => AnnAst.Tint
      | _         => raise TypeError


  (* typeOfExp takes in an AnnAst.exp and returns it's type.
   * This should probably move to AnnAst.sml
   *)
  fun typeOfExp ( e : AnnAst.exp) : AnnAst.typ =
    case e of
      AnnAst.EInt(i)          => AnnAst.Tint
   |  AnnAst.EDouble(d)       => AnnAst.Tdouble
   |  AnnAst.EString(s)       => AnnAst.Tstring
   |  AnnAst.ETrue            => AnnAst.Tbool
   |  AnnAst.EFalse           => AnnAst.Tbool
   |  AnnAst.EId(e1, t)       => t
   |  AnnAst.ECall(i, l, t)   => t
   |  AnnAst.EPostIncr(e1, t) => t
   |  AnnAst.EPostDecr(e1, t) => t
   |  AnnAst.EPreIncr(e1, t)  => t
   |  AnnAst.EPreDecr(e1, t)  => t
   |  AnnAst.EMul(e1, e2, t)  => t
   |  AnnAst.EDiv(e1, e2, t)  => t
   |  AnnAst.EMod(e1, e2, t)  => t
   |  AnnAst.EAdd(e1, e2, t)  => t
   |  AnnAst.ESub(e1, e2, t)  => t
   |  _                       => raise TypeError

  fun inferExp (e : Ast.exp, g : env ) : AnnAst.exp =
      case e of
        Ast.EInt(i)       => AnnAst.EInt(i)
      | Ast.EDouble(d)    => AnnAst.EDouble(d)
      | Ast.EString(s)    => AnnAst.EString(s)
      | Ast.ETrue         => AnnAst.ETrue
      | Ast.EFalse        => AnnAst.EFalse
      | Ast.EId(id)       => AnnAst.EId(id, findIdTyp (id, g))
      (* Unsure about ECall *)
      | Ast.ECall(id, l)  => AnnAst.EId(id, findIdTyp (id, g))
      | Ast.EPostIncr(id) => AnnAst.EPostIncr(id, ensureInt(id, g))
      | Ast.EPostDecr(id) => AnnAst.EPostDecr(id, ensureInt(id, g))
      | Ast.ENot(exp)     => (case inferExp(exp, g) of
                                AnnAst.ETrue  => AnnAst.ENot(inferExp(exp, g), AnnAst.Tbool)
                              | AnnAst.EFalse => AnnAst.ENot(inferExp(exp, g), AnnAst.Tbool)
                              | _             => raise TypeError )
      | Ast.EPreIncr(id)  => AnnAst.EPreIncr(id, ensureInt(id, g))
      | Ast.EPreDecr(id)  => AnnAst.EPreDecr(id, ensureInt(id, g))
      | Ast.EMul(e1, e2)  => AnnAst.EMul(inferExp(e1, g), inferExp(e2, g), ensureArithType(e1, e2, g))
      | Ast.EDiv(e1, e2)  => AnnAst.EDiv(inferExp(e1, g), inferExp(e2, g), ensureArithType(e1, e2, g))
      | Ast.EAdd(e1, e2)  => AnnAst.EAdd(inferExp(e1, g), inferExp(e2, g), ensureArithType(e1, e2, g))
      | Ast.ESub(e1, e2)  => AnnAst.ESub(inferExp(e1, g), inferExp(e2, g), ensureArithType(e1, e2, g))
      | _                 => raise TypeError

    and
    (* ensureArithType takes in two Ast.exps and checks to make sure
     * either both evaluate to an int, or both evaluate to a double
     *)
      ensureArithType (e1: Ast.exp, e2: Ast.exp, g: env) : AnnAst.typ =
        case typeOfExp(inferExp(e1, g)) of 
        AnnAst.Tint    => (case (typeOfExp(inferExp(e2, g))) of 
                              AnnAst.Tint     => AnnAst.Tint
                              |_              => raise TypeError)
      | AnnAst.Tdouble => (case (typeOfExp(inferExp(e2, g))) of 
                               AnnAst.Tdouble => AnnAst.Tdouble
                               |_             => raise TypeError)
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





