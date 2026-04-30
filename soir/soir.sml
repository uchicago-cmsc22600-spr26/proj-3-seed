(* soir.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure SOIR =
  struct

    datatype ty
      = OptionTy of ty                          (* optional value *)
      | ObjectTy                                (* pointer to heap-allocated object *)
      | ClassTy                                 (* pointer to class metadata *)
      | IndexTy                                 (* pointer to index table for interface view *)
      | FuncTy of ty list * ty                  (* code address *)
      | TupleTy of ty list                      (* tuple of values; includes wrapped objects *)
      | BoolTy                                  (* boolean *)
      | IntTy                                   (* integer *)
      | StringTy                                (* primitive string *)
      | VoidTy                                  (* void (return type only) *)
      | NilTy                                   (* the type of `NilVal` *)

    datatype program = PROGRAM of {
          classes : class list,                 (* list of classes defined in the program; plus
                                                 * any internal classes.
                                                 *)
          indices : index_table list,           (* list of index tables used in the program *)
          functions : func_def list,            (* list of class initialization and member
                                                 * functions.
                                                 *)
          main : func_def                       (* the "sool_main" function *)
        }

    and func_def = FuncDef of {                 (* a function definition *)
          f : func,                             (* the function variable *)
          params : var list,                    (* function parameters *)
          body : block                          (* function body *)
        }

    and block = Block of {
          lab : ProgPt.t,                       (* block label *)
          stms : stm list                       (* a block of statements *)
        }

    and stm
      = LabelStm of ProgPt.t * stm              (* statement labeled with program point *)
      | LocalVarStm of var * exp option         (* declare local with optional init *)
      | LocalAssignStm of var * exp             (* assign to local variable *)
      | AssignStm of value * mvar * exp         (* assign to member variable *)
      | NewStm of var * class                   (* `var obj = new c` -- object allocation *)
      | TupleStm of var * value list            (* `var tpl = <v, ...>` -- tuple allocation *)
      | ApplyStm of var * value * value list    (* `var x = f(v, ...)` -- function application *)
      | CallStm of value * value list           (* `f(v, ...)` -- void-function application *)
      | IfStm of value * block * block option   (* conditional *)
      | LoopStm of block                        (* loop; the structure of the loop
                                                 * should be:
                                                 *   loop {
                                                 *      <loop header>
                                                 *      exit_if v;
                                                 *      <loop body>
                                                 *   }
                                                 *)
      | ExitIfStm of value                      (* loop exit statement *)
      | ReturnStm of value option               (* return statement *)

    and exp
      = ValExp of value                         (* value *)
      | MemberVarExp of value * mvar            (* `v.x` -- member variable selection *)
      | MetaSel of value * item                 (* `v$f` -- select from metadata/index table *)
      | IndexSel of value * value               (* `v[v]` -- index metadata *)
      | PrimExp of PrimOp.t * value list        (* `p(args)` -- primitive operation *)
      | TupleSel of int * value                 (* `#i(v)` -- select from a tuple *)

  (* simple values, which are used as arguments *)
    and value
      = VarVal of var                           (* local variable *)
      | FuncVal of func                         (* function name *)
      | ClassVal of class                       (* reference to class metadata *)
      | IndexVal of index_table                 (* reference to interface index *)
      | IntVal of IntInf.int                    (* integer literal *)
      | BoolVal of bool                         (* boolean literal *)
      | StringVal of string                     (* string literal *)
      | NilVal                                  (* nil *)

    and var = VAR of {                          (* a local variable *)
          name : string,                        (* the variable's name *)
          stamp : Stamp.t,                      (* the variable's unique ID *)
          ty : ty                               (* the variable's type *)
        }

    and func = FUNC of {                        (* a function ID; unique to an implementation *)
          name : string,                        (* the function's name *)
          stamp : Stamp.t,                      (* the function's unique ID *)
          paramTys : ty list,                   (* parameter types *)
          retTy : ty                            (* return type *)
        }

    and class = CLASS of {
          tyc : Class.t,                        (* the AST class, which provides the class's name *)
          init : func,                          (* class initialization function *)
          vars : mvar list,                     (* member-variables (including inherited vars);
                                                 * these are ordered by increasing offset.
                                                 *)
          super : class option,                 (* the class that this class inherits from *)
          metadata : class_md_item list         (* the metadata items for this class; these
                                                 * items are ordered by increasing offset.
                                                 *)
        }

    and mvar = MVAR of {                        (* a member variable *)
          name : string,                        (* the variable's name *)
          ty : ty,                              (* the variable's type *)
          offset : int                          (* the variable's offset in its object *)
        }

    and class_md_item                           (* class metadata item *)
      = FuncItem of func                        (* either a member function *)
      | IndexItem of index_table                (* or an interface *)

    and index_table = INDEX of {                (* an interface index table *)
          cls : Class.t,                        (* the AST class that this is a view of *)
          ifc : Interface.t,                    (* the AST interface *)
          name : string,                        (* the table's name *)
          stamp : Stamp.t,                      (* the table's unique ID *)
          items : item list ref                 (* list of index items; a reference so that it
                                                 * can be filled in later.
                                                 *)
        }

    withtype item = string * int                (* item argument for MetaSel and item in IndexTbl.
                                                 * The string is the name of the item; the int is
                                                 * the offset.  The interpretation of the offset
                                                 * depends on contxt.  In a MetaSel(v,i), the
                                                 * offset is the index into `v`; in an IndexTbl
                                                 * the offset is the offset of the item in the
                                                 * wrapped object's class metadata.
                                                 *)

  end (* struct *)
