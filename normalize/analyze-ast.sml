(* analyze-ast.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure AnalyzeAST : sig

  (* per-class info *)
    type class_info = {
        size : int,                             (* the object size for the class *)
        vEnv : SOIR.mvar MemberVar.Map.map,     (* mapping from AST mvars to SOIR mvars *)
        fEnv : SOIR.func MemberFun.Map.map,     (* mapping from AST mfuns to SOIR funcs *)
        iEnv : SOIR.index_table Interface.Map.map, (* mapping from AST interfaces to the
                                                 * index table for this class.
                                                 *)
        md : SOIR.class                         (* class metadata *)
      }

  (* per-interface info *)
    type iface_info = {
        fEnv : SOIR.item MemberFun.Map.map,     (* mapping from AST mfuns to their offset
                                                 * in the index tables for this interface.
                                                 *)
        iEnv : SOIR.item Interface.Map.map      (* mapping from AST interfaces to their
                                                 * offset in the index tables for this
                                                 * interface.
                                                 *)
      }

  (* the information returned from analyzing the AST *)
    type info = {
        clsInfo : class_info Class.Tbl.hash_table,
        classes : SOIR.class list,
        ifcInfo : iface_info Interface.Tbl.hash_table,
        indices : SOIR.index_table list
      }

  (* analyse the AST representation of SooL program in preparation for translating
   * to SOIR.  The analysis computes object layouts, and the contents for class
   * metadata, and interface index tables.
   *)
    val analyze : AST.program -> info

  end = struct

    structure CTbl = Class.Tbl
    structure C = Coercions
    structure CSet = Coercions.Set
    structure ISet = Interface.Set
    structure CG = CoercionGraph
    structure Ty = Types
    structure IR = SOIR

  (* per-class info *)
    type class_info = {
        size : int,                             (* the object size for the class *)
        vEnv : IR.mvar MemberVar.Map.map,       (* mapping from AST mvars to SOIR mvars *)
        fEnv : IR.func MemberFun.Map.map,       (* mapping from AST mfuns to SOIR funcs *)
        iEnv : SOIR.index_table Interface.Map.map, (* mapping from AST interfaces to the index
                                                 * table for this class.
                                                 *)
        md : IR.class                           (* class metadata *)
      }

  (* per-interface info *)
    type iface_info = {
        fEnv : SOIR.item MemberFun.Map.map,     (* mapping from AST mfuns to their offset in the
                                                 * index tables for this interface.
                                                 *)
        iEnv : SOIR.item Interface.Map.map      (* mapping from AST interfaces to their offset in
                                                 * the index tables for this interface.
                                                 *)
      }

  (* the information returned from analyzing the AST *)
    type info = {
        clsInfo : class_info CTbl.hash_table,
        classes : SOIR.class list,
        ifcInfo : iface_info Interface.Tbl.hash_table,
        indices : SOIR.index_table list
      }

  (* metadata indices are offset by two to account for the size and super-class
   * fields that are at the beginning of the table.
   *)
    fun mdOffset i = i + 2

  (* map a primitive type to its internal class *)
    fun primClass Ty.BoolTy = Basis.boolCls
      | primClass Ty.IntTy = Basis.intCls
      | primClass Ty.StringTy = Basis.stringCls

  (* build the coercion graph from the set of object-to-object coercions from the AST.
   * At the same time, we build a mapping from classes to their declarations and compute
   * a set of the interfaces used in the program.
   *)
    fun buildCoercionGraph dcls = let
          val clsDclTbl = CTbl.mkTable (List.length dcls, Fail "class-dcl-table")
        (* add a coercion if it is an object-to-object coercion.  We ignore class-to-class
         * coercions, since we add coercions from subclasses to superclasses when we
         * process class declarations below.
         *)
          fun add (cs, Ty.OptTy to, Ty.OptTy from) = add (cs, to, from)
            | add (cs, Ty.IfcTy to, Ty.ClsTy from) = CSet.add(cs, C.IfcCls(to, from))
            | add (cs, Ty.IfcTy to, Ty.PrimTy from) = CSet.add(cs, C.IfcCls(to, primClass from))
            | add (cs, Ty.IfcTy to, Ty.IfcTy from) = CSet.add(cs, C.IfcIfc(to, from))
            | add (cs, _, _) = cs
          fun doExp (e, cs) = (case e
                 of AST.EXP_Absorb(e1, e2) => doExp (e2, doExp (e1, cs))
                  | AST.EXP_Orelse(e1, e2) => doExp (e2, doExp (e1, cs))
                  | AST.EXP_Andalso(e1, e2) => doExp (e2, doExp (e1, cs))
                  | AST.EXP_PrimOp(e1, _, e2) => doExp (e2, doExp (e1, cs))
                  | AST.EXP_New(_, args) => List.foldl doExp cs args
                  | AST.EXP_Apply{obj, args, ...} => List.foldl doExp cs (obj::args)
                  | AST.EXP_As(_, e) => doExp (e, cs)
                  | AST.EXP_Select{obj, ...} => doExp (obj, cs)
                  | AST.EXP_Require e => doExp (e, cs)
                  | AST.EXP_Var _ => cs
                  | AST.EXP_Number _ => cs
                  | AST.EXP_String _ => cs
                  | AST.EXP_Bool _ => cs
                  | AST.EXP_Nil _ => cs
                  | AST.EXP_Coerce{to, from, e} => add (doExp (e, cs), to, from)
                (* end case *))
          fun doStm (s, cs) = (case s
                 of AST.STM_Var(_, e) => doExp (e, cs)
                  | AST.STM_While(e, blk) => doBlk (blk, doExp (e, cs))
                  | AST.STM_If(conds, optBlk) =>
                      List.foldl (fn ((e, blk), cs) => doBlk (blk, doExp (e, cs)))
                        (Option.fold doBlk cs optBlk)
                          conds
                  | AST.STM_Return optE => Option.fold doExp cs optE
                  | AST.STM_Call{obj, args, ...} => List.foldl doExp cs (obj::args)
                  | AST.STM_FieldAssign{obj, rhs, ...} => doExp (rhs, doExp (obj, cs))
                  | AST.STM_VarAssign(_, e) => doExp (e, cs)
                (* end case *))
          and doBlk (AST.BLK stms, cs) = List.foldl doStm cs stms
          fun doVDcl (AST.VDCL(_, e), cs) = doExp(e, cs)
          fun doFDcl (AST.FDCL{body, ...}, cs) = doBlk (body, cs)
          fun doDcl (dcl as AST.DCL_Class{tyc, super, vars, funs, ...}, cs) = let
                val cs = (case super
                       of SOME(superTyc, args) => (* add coercion for super-class relation *)
                            CSet.add(List.foldl doExp cs args, C.ClsCls(superTyc, tyc))
                        | NONE => cs
                      (* end case *))
                in
                  CTbl.insert clsDclTbl (tyc, dcl);
                  List.foldl doFDcl (List.foldl doVDcl cs vars) funs
                end
            | doDcl (_, cs) = cs  (* no code, thus no coercions inside interfaces *)
        (* get the coercions from the AST *)
          val coercions = List.foldl doDcl CSet.empty dcls
        (* extract the set of interfaces that are used in the program from the coercions *)
          val iset = let
                fun add (C.IfcCls(ifc, _), iset) = ISet.add(iset, ifc)
                  | add (C.IfcIfc(ifc1, ifc2), iset) = ISet.add(ISet.add(iset, ifc1), ifc2)
                  | add (_, iset) = iset
                in
                  CSet.foldl add ISet.empty coercions
                end
          in
          (* add internal classes to clsDclTbl *)
            List.app (fn (dcl as AST.DCL_Class{tyc, ...}) => CTbl.insert clsDclTbl (tyc, dcl)) [
                InternalClasses.boolCls,
                InternalClasses.intCls,
                InternalClasses.stringCls,
                InternalClasses.systemCls
              ];
            (CG.new coercions, clsDclTbl, iset)
          end

    fun analyze (AST.PROGRAM{main, dcls}) = raise Fail "YOUR CODE HERE"

  end

