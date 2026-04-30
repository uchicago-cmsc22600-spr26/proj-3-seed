(* env.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *)

structure Env : sig

    type t

  (* create the top-level environment for translating from AST to SOIR *)
    val new : AnalyzeAST.info -> t

  (* bind an AST local variable to a SOIR variable *)
    val bindVar : t * LocalVar.t * SOIRVar.t -> t

  (* lookup a local variable in the environment *)
    val lookupVar : t * LocalVar.t -> SOIRVar.t

  (* lookup a class in the environment *)
    val lookupClass : t * Class.t -> SOIR.class

  (* lookup an interface in the environment *)
    val lookupIfcInfo : t * Interface.t -> AnalyzeAST.iface_info

  (* lookup a member-variable for class in the environment *)
    val lookupMVar : t * Class.t * MemberVar.t -> SOIR.mvar

  (* lookup a member-function for class in the environment returning the SOIR.func *)
    val lookupMFun : t * Class.t * MemberFun.t -> SOIR.func

  (* lookup the index table for a class and interface *)
    val lookupIndexTable : t * Class.t * Interface.t -> SOIR.index_table

  end = struct

    structure IR = SOIR
    structure VMap = LocalVar.Map
    structure MVar = MemberVar
    structure MFun = MemberFun

  (* environment use to translate AST to SOIR *)
    datatype t = E of {
        info : AnalyzeAST.info,         (* information about classes and interfaces *)
        vEnv : SOIR.var VMap.map        (* local-variable bindings *)
      }

  (* create the top-level environment for translating from AST to SOIR *)
    fun new info = E{
            info = info,
            vEnv = VMap.singleton(Basis.systemVar, SOIRBasis.systemVar)
          }

  (* bind an AST local variable to a SOIR variable *)
    fun bindVar (E{info, vEnv}, x, x') = E{info = info, vEnv = VMap.insert(vEnv, x, x')}

  (* lookup a local variable in the environment *)
    fun lookupVar (E{vEnv, ...}, x) = (case VMap.find(vEnv, x)
           of SOME x' => x'
            | NONE => raise Fail(concat["unable to find ", LocalVar.nameOf x, " in environment"])
          (* end case *))

    local
      fun lookupCls (E{info, ...}, cls) = (case Class.Tbl.find (#clsInfo info) cls
             of SOME clsInfo => clsInfo
              | NONE => raise Fail(concat["class ", Class.nameOf cls, " not found"])
            (* end case *))
      fun lookupIfc (E{info, ...}, ifc) = (case Interface.Tbl.find (#ifcInfo info) ifc
             of SOME ifcInfo => ifcInfo
              | NONE => raise Fail(concat["interface ", Interface.nameOf ifc, " not found"])
            (* end case *))
    in

  (* lookup a class in the environment *)
    fun lookupClass (env, cls) = #md (lookupCls(env, cls))

  (* lookup an interface in the environment *)
    fun lookupIfcInfo (env, ifc) = lookupIfc (env, ifc)

  (* lookup a member-variable for class in the environment *)
    fun lookupMVar (env, cls, mvar) = (case MVar.Map.find(#vEnv (lookupCls(env, cls)), mvar)
           of SOME mv => mv
            | NONE => raise Fail(concat[
                  "member variable ", Class.nameOf cls, ".", MVar.nameOf mvar, " not found"
                ])
          (* end case *))

  (* lookup a member-function for class in the environment returning the SOIR.func *)
    fun lookupMFun (env, cls, mfun) = (case MFun.Map.find(#fEnv (lookupCls(env, cls)), mfun)
           of SOME mf => mf
            | NONE => raise Fail(concat[
                  "member function", Class.nameOf cls, ".", MFun.nameOf mfun, " not found"
                ])
          (* end case *))

  (* lookup the index table for a class and interface *)
    fun lookupIndexTable (env, cls, ifc) = (
          case Interface.Map.find(#iEnv (lookupCls (env, cls)), ifc)
           of SOME tbl => tbl
            | NONE => raise Fail(concat[
                  "table ", Class.nameOf cls, "@", Interface.nameOf ifc, " not found"
                ])
          (* end case *))

    end (* local *)

  end

