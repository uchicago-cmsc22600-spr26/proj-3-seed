(* ast.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Typed Abstract Syntax Tree for SooL.
 *)

structure AST =
  struct

  (* local variables *)
    type var = LocalVar.t

  (* class/interface members *)
    type memb_var = MemberVar.t
    type memb_fun = MemberFun.t

  (* types *)
    datatype typ = datatype Types.t

    datatype program = PROGRAM of {
        main : Types.class_tyc,                 (* the main class of the program *)
        dcls : dcl list                         (* the declarations *)
      }

  (* top-level dclarations *)
    and dcl
      = DCL_Class of {                          (* class declaration *)
            tyc : Types.class_tyc,                (* class type constructor *)
            params : var list,                    (* constructor parameters *)
            super : (Types.class_tyc * exp list) option,
                                                  (* super-class initialization *)
            vars : var_dcl list,                  (* instance variables *)
            funs : fun_dcl list                   (* member-function implementations *)
          }
      | DCL_Interface of Types.interface_tyc    (* interface declaration *)

    and var_dcl = VDCL of memb_var * exp        (* member variable dcl *)

    and fun_dcl = FDCL of {                     (* class-member function (aka method) dcl *)
            override : bool,                      (* true for 'override' methods *)
            name : memb_fun,                      (* the function's name *)
            self : var,                           (* the implicit self parameter *)
            params : var list,                    (* the function's parameters *)
            retTy : typ,                          (* the function's return type *)
            body : blk                            (* the function's body *)
          }

    and blk = BLK of stm list                   (* block *)

    and stm
      = STM_Var of var * exp                    (* local variable dcl *)
      | STM_While of exp * blk                  (* while loop *)
      | STM_If of (exp * blk) list * blk option (* conditional *)
      | STM_Return of exp option                (* 'return' <exp>? *)
      | STM_Call of mfun_call                   (* member-function call *)
      | STM_FieldAssign of {                    (* field assignment *)
            obj : exp,                            (* the object *)
            cls : Class.t,                        (* class of type of object *)
            var : memb_var,                       (* the member function *)
            rhs : exp                             (* the rhs of the assignment *)
          }
      | STM_VarAssign of var * exp              (* local-variable assignment *)

    and exp
      = EXP_Absorb of exp * exp                 (* <exp> '??' <exp> *)
      | EXP_Orelse of exp * exp                 (* <exp> '||' <exp> *)
      | EXP_Andalso of exp * exp                (* <exp> '&&' <exp> *)
      | EXP_PrimOp of exp * BinOp.t * exp       (* primitive operation *)
      | EXP_New of Types.class_tyc * exp list   (* object construction *)
      | EXP_Apply of mfun_call                  (* member-function application *)
      | EXP_As of Types.class_tyc * exp         (* downcast *)
      | EXP_Nil of typ                          (* <tyc> '.' 'nil' *)
      | EXP_Select of {                         (* member-variable selection *)
            obj : exp,                            (* the object *)
            cls : Class.t,                        (* class of type of object *)
            opt : bool,                           (* optional select *)
            var : memb_var                        (* the member function *)
          }
      | EXP_Require of exp                      (* <exp> '!' *)
      | EXP_Var of var                          (* <id> or 'self'*)
      | EXP_Number of IntInf.int                (* <num> *)
      | EXP_String of string                    (* <str> *)
      | EXP_Bool of bool                        (* 'true' or 'false' *)
      | EXP_Coerce of {                         (* subtype to supertype coercion *)
            from : typ, to : typ, e : exp
          }

    withtype mfun_call = {                      (* member-function invocation *)
          obj : exp,                              (* the object *)
          objTy : Types.t,                        (* type of object (class or interface) *)
          opt : bool,                             (* optional apply *)
          f : memb_fun,                           (* the member function *)
          args : exp list                         (* arguments *)
        }

  end
