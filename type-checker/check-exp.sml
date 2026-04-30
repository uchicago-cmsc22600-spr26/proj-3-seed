(* check-exp.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Type checking for SooL expressions.
 *)

structure CheckExp : sig

  (* check the type of an expression; we return the AST and type of the expression *)
    val check : Env.t * ParseTree.exp -> AST.exp * Types.t

  (* `expectTy (env, expectedTy, (e, ty))` check if the result of type checking
   * an expression `e, ty)` is equal to or a subtype of `expectedTy`.  If
   * necessary, we add a coercion from subtype to supertype.
   *)
    val expectTy : Env.t * Types.t * ParseTree.exp -> AST.exp

  (* check a list of argument expressions against the list of expected types *)
    val checkArgs : Env.t * Types.t list * ParseTree.exp list -> AST.exp list

  end = struct

    structure PT = ParseTree
    structure Ty = Types
    structure Err = TypeError

  (* import the token datatype to get access to the constructors without qualification *)
    datatype token = datatype Err.token

  (* terms to return when there are errors and we cannot generate AST *)
    val bogusExp = AST.EXP_Nil Ty.ErrorTy
    val bogusExpTy = (bogusExp, Ty.ErrorTy)
    val bogusMVar = MemberVar.new(Atom.atom "*bogus*", Ty.ErrorTy)
    val bogusMFun = MemberFun.new(Atom.atom "*bogus*", [], Ty.ErrorTy)

    (* `coerce {from, to, e}` checks to see `from` is a subtype of `to`; if so, it
     * introduces an upcast coercion around the expression `e`.  We assume that `from`
     * is the actual type of `e` and `to` is the expected type.
     *)
    fun coerce (arg as {from, to, e}) = if TypeUtil.subtype(from, to)
          then  AST.EXP_Coerce arg
          else e

    (* typecheck the expression `e` where we expect it to have `expectedTy` as
     * its type.  If its type is a subtype of the expected type, then we insert
     * an upcast coercion.
     *)
    fun expectTy (env, expectedTy, e) = let
          val (e, ty) = check (env, e)
          in
            case TypeUtil.compare(ty, expectedTy)
             of TypeUtil.EQUTY => e
              | TypeUtil.SUBTY => AST.EXP_Coerce{from=ty, to=expectedTy, e=e}
              | TypeUtil.MISMATCH => (
                  Err.error (env, [
                      S "expression does not have subtype of expected type\n",
                      S "  expected: ", TY expectedTy, S "\n",
                      S "  found:    ", TY ty
                    ]);
                  e)
            (* end case *)
          end

    and checkArgs (env, paramTys, args) = let
          fun chk ([], [], args') = List.rev args'
            | chk (typ::typs, arg::args, args') = let
                val arg' = expectTy (env, typ, arg)
                in
                  chk (typs, args, arg'::args')
                end
            | chk (_, _, args') = (
                Err.error (env, [S "arity mismatch in application"]);
                List.rev args')
          in
            chk (paramTys, args, [])
          end

    and check (env, exp) = (case exp
           of PT.EXP_Mark{span, tree} => check (Env.setSpan(env, span), tree)
            | PT.EXP_Absorb(e1, e2) => (case check (env, e1)
                 of (e1', Ty.OptTy ty1) => let
                      val (e2', ty2) = check (env, e2)
                      in
                        case TypeUtil.join(ty1, ty2)
                         of NONE => (
                              Err.error (env, [
                                  S "incomparable type for '??' operator\n",
                                  S "  lhs: ", TY(Ty.OptTy ty1), S "\n",
                                  S "  rhs: ", TY ty2
                                ]);
                              bogusExpTy)
                          | SOME ty => let
                              val e1' = coerce{from=ty1, to=ty, e=e1'}
                              val e2' = coerce{from=ty2, to=ty, e=e2'}
                              in
                                (AST.EXP_Absorb(e1', e2'), ty)
                              end
                        (* end case *)
                      end
                  | (_, ty1) => let
                      val (_, ty2) = check (env, e2)
                      in
                        Err.error (env, [
                            S "excepted option type for lhs argument to '??', but found ",
                            TY ty1
                          ]);
                        (bogusExp, ty2)
                      end
                (* end case *))
            | PT.EXP_Orelse(e1, e2) => let
                val e1' = expectTy (env, Ty.boolTy, e1)
                val e2' = expectTy (env, Ty.boolTy, e2)
                in
                  (AST.EXP_Orelse(e1', e2'), Ty.boolTy)
                end
            | PT.EXP_Andalso(e1, e2) => let
                val e1' = expectTy (env, Ty.boolTy, e1)
                val e2' = expectTy (env, Ty.boolTy, e2)
                in
                  (AST.EXP_Andalso(e1', e2'), Ty.boolTy)
                end
            | PT.EXP_BinOp(e1, rator, e2) => if BinOp.isEqOp rator
                then let
                  val (e1', ty1) = check (env, e1)
                  val (e2', ty2) = check (env, e2)
                  in
                  (* check tha validity of the arguments *)
                    if TypeUtil.subtype(ty1, ty2) orelse TypeUtil.subtype(ty2, ty1)
                      then ()
                      else Err.error (env, [
                          S "invalid argument types for '", S(BinOp.toString rator),
                          S "' operator: ", TYS[ty1, ty2]
                        ]);
                    (AST.EXP_PrimOp(e1', rator, e2'), Ty.boolTy)
                  end
                else let
                  val (ty1, ty2, resTy) = BasisEnv.typeOfBinOp rator
                  val e1' = expectTy (env, ty1, e1)
                  val e2' = expectTy (env, ty2, e2)
                  in
                    (AST.EXP_PrimOp(e1', rator, e2'), resTy)
                  end
            | PT.EXP_NegOp e => let
                val e' = expectTy (env, Ty.intTy, e)
                in
                  (AST.EXP_PrimOp(AST.EXP_Number 0, BinOp.^-^, e'), Ty.intTy)
                end
            | PT.EXP_New(cls, args) => (case Env.findTyc (env, #tree cls)
                 of SOME(Ty.ClsTy cls) => let
                      val paramTys = Class.paramTypesOf cls
                      val args' = checkArgs (env, paramTys, args)
                      in
                        (AST.EXP_New(cls, args'), Ty.ClsTy cls)
                      end
                  | _ => (
                      Err.error (env, [
                          S "unknown class ", A(#tree cls), S " in object construction"
                        ]);
                      bogusExpTy)
                (* end case *))
            | PT.EXP_Apply(memb, args) => let
                val (obj, objTy, isOpt, mfun) = checkMemberFun (env, memb)
                val Ty.FUNTY(paramTys, resTy) = MemberFun.typeOf mfun
                val args' = (case resTy
                       of Ty.ErrorTy => [] (* avoid cascading error messages *)
                        | _ => checkArgs (env, paramTys, args)
                      (* end case *))
                val exp = AST.EXP_Apply{
                        obj = obj, objTy = objTy, opt = isOpt, f = mfun, args = args'
                      }
                in
                  (exp, if isOpt then TypeUtil.optionTy resTy else resTy)
                end
            | PT.EXP_As(cls, e) => let
                val (e', ty) = check (env, e)
                in
                  case Env.findClass (env, #tree cls)
                   of SOME cls' => if TypeUtil.subtype(Ty.ClsTy cls', ty)
                        then (AST.EXP_As(cls', e'), Ty.ClsTy cls')
                        else (
                          Err.error (env, [
                              S "type of argument ", TY ty, S " is not supertype of ",
                              A(#tree cls), S "in 'as' expression"
                            ]);
                          bogusExpTy)
                    | NONE => (
                      Err.error (env, [
                          S "unknown class ", A(#tree cls), S " in `as` expression"
                        ]);
                      bogusExpTy)
                  (* end case *)
                end
            | PT.EXP_Nil typ => let
                val typ = CheckType.check (env, typ)
                in
                  (AST.EXP_Nil typ, Ty.OptTy typ)
                end
            | PT.EXP_Select memb => let
                val (obj, objTy, isOpt, mvar) = checkMemberVar (env, memb)
                val ty = MemberVar.typeOf mvar
                val exp = (case objTy
                       of Ty.ClsTy cls => AST.EXP_Select{
                              obj = obj, cls = cls, opt = isOpt, var = mvar
                            }
                        | _ => bogusExp
                      (* end case *))
                in
                  (exp, if isOpt then TypeUtil.optionTy ty else ty)
                end
            | PT.EXP_Require e => (case check (env, e)
                 of (e', Ty.OptTy ty) => (AST.EXP_Require e', ty)
                  | (e', ty) => (
                      Err.error (env, [
                          S "excepted option type for argument to '!', but found ", TY ty
                        ]);
                      bogusExpTy)
                (* end case *))
            | PT.EXP_Var x => (case Env.findVar(env, #tree x)
                 of SOME x' => (AST.EXP_Var x', LocalVar.typeOf x')
                  | NONE => (
                      Err.error (Env.setSpan(env, #span x), [
                          S "reference to undeclared variable ", A(#tree x)
                        ]);
                      bogusExpTy)
                (* end case *))
            | PT.EXP_Number n => (
                (* NOTE: since the "-" sign is not part of the literal (see
                 * the Project 1 description), all numbers are positive at
                 * this stage.
                 *)
                if (4611686018427387903 < n)
                  then Err.error (env, [S "integer literal is too large"])
                  else ();
                (AST.EXP_Number n, Ty.intTy))
            | PT.EXP_String s => (AST.EXP_String s, Ty.stringTy)
            | PT.EXP_Bool b => (AST.EXP_Bool b, Ty.boolTy)
          (* end case *))

  (* check the object expression part of a member selection.  Return the AST for the
   * expression, the type of the object (with any '?' stripped off), a boolean that
   * is true for '?' selectors, and the name of the member.
   *)
    and chkObjExp (env, memb) = (case memb
           of PT.MEMB_Mark{span, tree} => chkObjExp (Env.setSpan(env, span), tree)
            | PT.MEMB(e, sel, x) => let
                val (e, ty) = check (env, e)
              (* if necessary, coerce primitive types to their object interface *)
                val (e, ty) = (case TypeUtil.primToObjectTy ty
                       of SOME ty' => (AST.EXP_Coerce{from=ty, to=ty', e=e}, ty')
                        | NONE => (e, ty)
                      (* end case *))
              (* check the selector *)
                val (e, ty, isOpt) = (case (sel, ty)
                       of (PT.DOT, Ty.OptTy ty') => (
                            Err.error (env, [
                                S "expected non-optional type for '.",
                                S(Atom.toString(#tree x)), S "', but found", TY ty
                              ]);
                            (e, ty', false))
                        | (PT.DOT, _) => (e, ty, false)
                        | (PT.REQ, Ty.OptTy ty') => (AST.EXP_Require e, ty', false)
                        | (PT.REQ, _) => (
                            Err.error (env, [
                                S "expected optional type for '!",
                                S(Atom.toString(#tree x)), S "', but found", TY ty
                              ]);
                            (e, ty, false))
                        | (PT.OPT, Ty.OptTy ty') => (e, ty', true)
                        | (PT.OPT, _) => (
                            Err.error (env, [
                                S "expected optional type for '?",
                                S(Atom.toString(#tree x)), S "', but found", TY ty
                              ]);
                            (e, ty, true))
                      (* end case *))
                in
                  (e, ty, isOpt, #tree x)
                end
          (* end case *))

  (* check an object member-variable selection; return a triple `(e, isOpt, x)`,
   * where `e` is the object expression, `isOpt` is true for `?` selections,
   * and `x` is the member variable.  Note that `!` selections are turned into
   * `EXP_Require` expressions and will have `false` for `isOpt`.
   *)
    and checkMemberVar (env, memb) = let
          val (obj, objTy, isOpt, x) = chkObjExp (env, memb)
          in
            case objTy
             of Ty.ClsTy cls => (case Class.findVar (cls, x)
                   of SOME x' => let
                        val cls' = Env.getCurrentClass env
                        in
                        (* check visibility of x' *)
                          if TypeUtil.subtype(Ty.ClsTy cls', Ty.ClsTy cls)
                            then (obj, objTy, isOpt, x')
                            else (
                              Err.error(env, [
                                  S "member variable ", A x, S " of class '", S(Class.nameOf cls),
                                  S "' is not visible in class '", S(Class.nameOf cls'), S "'"
                                ]);
                              (bogusExp, objTy, isOpt, bogusMVar))
                        end
                    | NONE => (
                        Err.error (env, [
                            S "class ", S(Class.nameOf cls),
                            S " does not have a variable named ", A x
                          ]);
                        (bogusExp, objTy, isOpt, bogusMVar))
                  (* end case *))
              | Ty.ErrorTy => (bogusExp, Ty.ErrorTy, isOpt, bogusMVar)
              | ty => (
                  Err.error (env, [
                      S "expected class type for selection of variable ", A x,
                      S ", but found ", TY ty
                    ]);
                  (bogusExp, Ty.ErrorTy, isOpt, bogusMVar))
            (* end case *)
          end

  (* check an object member-function selection; return a triple `(e, isOpt, f)`,
   * where `e` is the object expression, `isOpt` is true for `?` selections,
   * and `f` is the member function.  Note that `!` selections are turned into
   * `EXP_Require` expressions and will have `false` for `isOpt`.
   *)
    and checkMemberFun (env, memb) = let
          val (obj, objTy, isOpt, f) = chkObjExp (env, memb)
          in
            case objTy
             of Ty.ClsTy cls => (case Class.findFun (cls, f)
                   of SOME f' => (obj, objTy, isOpt, f')
                    | NONE => (
                        Err.error (env, [
                            S "class ", S(Class.nameOf cls),
                            S " does not have a function named ", A f
                          ]);
                        (bogusExp, objTy, isOpt, bogusMFun))
                  (* end case *))
              | Ty.IfcTy ifc => (case Interface.findFun (ifc, f)
                   of SOME f' => (obj, objTy, isOpt, f')
                    | NONE => (
                        Err.error (env, [
                            S "interface ", S(Interface.nameOf ifc),
                            S " does not have a function named ", A f
                          ]);
                        (bogusExp, objTy, isOpt, bogusMFun))
                  (* end case *))
              | Ty.ErrorTy => (bogusExp, Ty.ErrorTy, isOpt, bogusMFun)
              | ty => (
                  Err.error (env, [
                      S "expected object type for selection of function ", A f,
                      S ", but found ", TY ty
                    ]);
                  (bogusExp, Ty.ErrorTy, isOpt, bogusMFun))
            (* end case *)
          end

  end
