(* check-block.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Type checker for SooL blocks and statements.
 *)

structure CheckBlock : sig

  (* typecheck a block of statements in the given environment; return the AST block *)
    val check : Env.t * ParseTree.blk -> AST.blk

  end = struct

    structure PT = ParseTree
    structure Err = TypeError

  (* import the token datatype to get access to the constructors without qualification *)
    datatype token = datatype Err.token

  (* terms to return when there are errors and we cannot generate AST *)
    val bogusStm = AST.STM_Return NONE

    fun check (env, blk) = (case blk
           of PT.BLK_Mark{span, tree} => check (Env.setSpan(env, span), tree)
            | PT.BLK stms => let
                fun chk (env, [], stms') = AST.BLK(List.rev stms')
                  | chk (env, stm::stms, stms') = let
                      val (stm', env) = chkStm (env, stm)
                      in
                         chk (env, stms, stm'::stms')
                      end
                in
                  chk (env, stms, [])
                end
          (* end case *))

    and chkStm (env, stm) = (case stm
           of PT.STM_Mark{span, tree} => chkStm (Env.setSpan(env, span), tree)
            | PT.STM_Var(x, e) => let
                val (e', typ) = CheckExp.check (env, e)
                val x' = LocalVar.new (#tree x, typ)
                in
                  (AST.STM_Var(x', e'), Env.insertVar (env, #tree x, x'))
                end
            | PT.STM_While(e, body) => let
                val e' = CheckExp.expectTy (env, Types.boolTy, e)
                val body' = check (env, body)
                in
                  (AST.STM_While(e', body'), env)
                end
            | PT.STM_If(conds, optElse) => let
                fun chkIf {span, tree=(e, blk)} = let
                      val env = Env.setSpan(env, span)
                      in
                        (CheckExp.expectTy (env, Types.boolTy, e), check (env, blk))
                      end
                val stm = AST.STM_If(
                      List.map chkIf conds,
                      Option.map (fn blk => check (env, blk)) optElse)
                in
                  (stm, env)
                end
            | PT.STM_Return NONE => (case Env.getReturnType env
                 of Types.VoidTy => (AST.STM_Return NONE, env)
                  | _ => (
                      Err.error (env, [S "expected expression for 'return' in non-void function"]);
                      (AST.STM_Return NONE, env))
                (* end case *))
            | PT.STM_Return(SOME e) => (case Env.getReturnType env
                 of Types.VoidTy => (
                      Err.error (env, [S "unexpected expression for 'return' in void function"]);
                      (AST.STM_Return NONE, env))
                  | retTy => let
                      val e' = CheckExp.expectTy (env, retTy, e)
                      in
                        (AST.STM_Return(SOME e'), env)
                      end
                (* end case *))
            | PT.STM_Call e => (case CheckExp.check (env, e)
                 of (AST.EXP_Apply call, Types.VoidTy) => (AST.STM_Call call, env)
                  | (AST.EXP_Apply call, Types.ErrorTy) => (AST.STM_Call call, env)
                  | (AST.EXP_Apply call, ty) => (
                      Err.error (env, [
                          S "expected 'void' return type for function call statement, but found",
                          TY ty
                        ]);
                      (AST.STM_Call call, env))
                  | _ => (
                      Err.error (env, [
                          S "syntax error; expression statement is not an application"
                        ]);
                      (bogusStm, env))
                (* end case *))
            | PT.STM_FieldAssign(e1, e2) => (case CheckExp.check (env, e1)
                 of (AST.EXP_Select{obj, cls, opt=false, var}, ty) => let
                      val rhs = CheckExp.expectTy (env, ty, e2)
                      in
                        (AST.STM_FieldAssign{obj=obj, cls=cls, var=var, rhs=rhs}, env)
                      end
                  | (AST.EXP_Select{opt=true, ...}, ty) => (
                      Err.error (env, [S "illegal assignment to optional selection of variable"]);
                      (bogusStm, env))
                  | _ => (
                      Err.error (env, [
                          S "syntax error; lhs of assignment is not a member-variable selection"
                        ]);
                      (bogusStm, env))
                (* end case *))
            | PT.STM_VarAssign(x, e) => (case Env.findVar(env, #tree x)
                 of SOME x' => let
                      val e' = CheckExp.expectTy(env, LocalVar.typeOf x', e)
                      in
                        (AST.STM_VarAssign(x', e'), env)
                      end
                  | NONE => (
                      Err.error (Env.setSpan(env, #span x), [
                          S "assignment to undeclared variable ", A(#tree x)
                        ]);
                      (bogusStm, env))
                (* end case *))
          (* end case *))

  end
