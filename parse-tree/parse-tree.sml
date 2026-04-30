(* parse-tree.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Parse-tree IR for Sool.
 *)

structure ParseTree =
  struct

  (* attach source-location information to a tree *)
    type 'a mark = 'a Error.mark

  (* an identifier with location info *)
    type id = Atom.atom mark

  (* a SooL program is a list of declarations *)
    datatype program
      = PROGRAM of dcl list mark

  (* top-level dclarations *)
    and dcl
      = DCL_Mark of dcl mark
      | DCL_Class of {                          (* 'class' <id> ... *)
            name : id,                          (* class name *)
            params:  param list,                (* constructor parameters *)
            super : (id * exp list) option,     (* super-class initialization *)
            vars : var_dcl list,                (* instance variables *)
            meths : meth_dcl list               (* method implementations *)
          }
      | DCL_Interface of {                      (* 'interface' <id> ... *)
            name : id,                          (* interface-type name *)
            extends : id option,                (* optional interface that this extends *)
            meths : meth_spc list               (* method specifications *)
          }

  (* class and method parameters *)
    and param
      = PARAM_Mark of param mark
      | PARAM of id * typ                       (* <id> ':' <typ> *)

  (* types of all kinds (types, type constructors, and return types) *)
    and typ
      = TY_Mark of typ mark
      | TY_Option of typ                        (* <typ> '?' *)
      | TY_Id of id                             (* class or interface type *)
      | TY_Bool                                 (* 'bool' *)
      | TY_Int                                  (* 'int' *)
      | TY_String                               (* 'string' *)
      | TY_Void                                 (* 'void' *)

    and var_dcl
      = VDCL_Mark of var_dcl mark
      | VDCL of id * typ * exp                  (* 'var' <id> ':' <typ> '=' <exp> *)

  (* class-member function (aka method) declaration *)
    and meth_dcl
      = MDCL_Mark of meth_dcl mark
      | MDCL of {
            override : bool,                    (* true for 'override' methods *)
            name : id,                          (* the function's name *)
            params : param list,                (* the function's parameters *)
            retTy : typ,                        (* the function's return type *)
            body : blk                          (* the function's body *)
          }

  (* function specification in an interface *)
    and meth_spc
      = MSPC_Mark of meth_spc mark
      | MSPC of {
            name : id,                          (* the function's name *)
            paramTys : typ list,                (* the function's parameter types *)
            retTy : typ                         (* the function's return type *)
          }

    and exp
      = EXP_Mark of exp mark
      | EXP_Absorb of exp * exp                 (* <exp> '??' <exp> *)
      | EXP_Orelse of exp * exp                 (* <exp> '||' <exp> *)
      | EXP_Andalso of exp * exp                (* <exp> '&&' <exp> *)
      | EXP_BinOp of exp * BinOp.t * exp        (* <exp> <binop> <exp> *)
      | EXP_NegOp of exp                        (* '-' <exp> *)
      | EXP_New of id * exp list                (* <id> '(' <exp>* ')' *)
      | EXP_Apply of member * exp list          (* method application *)
      | EXP_As of id * exp                      (* <cls> '.' 'as' '(' <exp> ')' *)
      | EXP_Nil of typ                          (* <tyc> '.' 'nil' *)
      | EXP_Select of member                    (* field selection *)
      | EXP_Require of exp                      (* <exp> '!' *)
      | EXP_Var of id                           (* <id> or 'self'*)
      | EXP_Number of IntInf.int                (* <num> *)
      | EXP_String of string                    (* <str> *)
      | EXP_Bool of bool                        (* 'true' or 'false' *)

  (* selection of object member-variables and functions *)
    and member
      = MEMB_Mark of member mark
      | MEMB of exp * select * id               (* object-member selection *)

    and select = DOT | REQ | OPT                (* select ops: '.', '!', or '?' *)

    and blk
      = BLK_Mark of blk mark
      | BLK of stm list                         (* '{' <stm>* '}' *)

    and stm
      = STM_Mark of stm mark
      | STM_Var of id * exp                     (* 'var' <id> '=' <exp> *)
      | STM_While of exp * blk                  (* 'while' <exp> <blk> *)
      | STM_If of (exp * blk) mark list * blk option
                                                (* 'if' <exp> 'then' <blk> ... *)
      | STM_Return of exp option                (* 'return' <exp>? *)
      | STM_Call of exp                         (* object creation or application *)
      | STM_FieldAssign of exp * exp            (* <exp> ':=' <exp> where the lhs denotes a member *)
      | STM_VarAssign of id * exp               (* <id> ':=' <exp> *)

  end
