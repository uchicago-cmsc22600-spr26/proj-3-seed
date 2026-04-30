(* env.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure Env : sig

  (* An Env.t tracks context information for the type checker, including the current
   * span, the enclosing class (if any), the global class and interface environments,
   * and the variable environment.
   *)
    type t

  (* create a new environment that is initialized to the SooL Basis Environment *)
    val new : Error.err_stream * Error.span -> t

  (* set the current span *)
    val setSpan : t * Error.span -> t

  (* get the current context information for reporting errors  *)
    val context : t -> Error.err_stream * Error.span

  (* have any errors been reported? *)
    val anyErrors : t -> bool

  (* record the current class *)
    val setCurrentClass : t * Class.t -> t

  (* get the current class; this will raise the Fail exception if there isn't a current class *)
    val getCurrentClass : t -> Class.t

  (* set the return type for the current method *)
    val setReturnType : t * Types.t -> t

  (* get the return type for the current method *)
    val getReturnType : t -> Types.t

  (* insert class/interface types *)
    val insertClass : t * Atom.atom * Class.t -> unit
    val insertIface : t * Atom.atom * Interface.t -> unit

  (* search for a class/interface type in the environment.  Return `NONE` if it
   * is not found.  Otherwise return `SOME typ`, where `typ` is either a class
   * type or an interface types.
   *)
    val findTyc : t * Atom.atom -> Types.t option

  (* search for a class in the environment.  Return `NONE` if it is not found.
   * Otherwise return `SOME cls`, where `cls` is either a class
   * type or an interface types.
   *)
    val findClass : t * Atom.atom -> Class.t option

  (* lookup class/interface type constructor, where we know that it was inserted
   * previously, so we are guaranteed to get something from the environment.
   *)
    val lookupClass : t * Atom.atom -> Class.t
    val lookupIface : t * Atom.atom -> Interface.t

  (* insert a local variable binding into the environment *)
    val insertVar : t * Atom.atom * LocalVar.t -> t

  (* search for a variable; return `NONE` if it is not found *)
    val findVar : t * Atom.atom -> LocalVar.t option

  end = struct

    structure AMap = AtomMap
    structure ATbl = AtomTable

  (* the environment is represented as a functional data structure (this datatype) with
   * an imperative component to track the global environment (the info field).
   *)
    datatype t = E of {
        span : Error.span,              (* current span in program source code *)
        vEnv : LocalVar.t AMap.map,     (* local-variable environment *)
        myClass : Class.t option,       (* the enclosing class (if any) *)
        retTyp : Types.t,               (* return type of current method *)
        info : info                     (* global info *)
      }

  (* imperative part of environment *)
    and info = Info of {
        clsEnv : Class.t ATbl.hash_table,       (* class types *)
        ifcEnv : Interface.t ATbl.hash_table,   (* interface types *)
        err : Error.err_stream                  (* for reporting errors *)
       }

  (* create a new environment that is initialized to the SooL Basis Environment *)
    fun new (errStrm, span) = let
          val info = Info{
                  clsEnv = BasisEnv.classEnv(),
                  ifcEnv = BasisEnv.interfaceEnv(),
                  err = errStrm
                }
          in
            E{  span = span, vEnv = BasisEnv.varEnv, myClass = NONE,
                retTyp = Types.ErrorTy, info = info
              }
          end

  (* set the current span *)
    fun setSpan (E{vEnv, myClass, retTyp, info, ...}, span) =
          E{span=span, vEnv=vEnv, myClass = myClass, retTyp = retTyp, info=info}

  (* get the current context information for reporting errors  *)
    fun context (E{span, info=Info{err, ...}, ...}) = (err, span)

    fun anyErrors (E{info=Info{err, ...}, ...}) = Error.anyErrors err

  (* record the current class *)
    fun setCurrentClass (E{span, vEnv, myClass=NONE, retTyp, info}, cls) =
          E{span = span, vEnv = vEnv, myClass = SOME cls, retTyp = retTyp, info = info}
      | setCurrentClass _ = raise Fail "setCurrentClass: there is already a current class"

  (* get the current class; this will raise the Fail exception if there isn't a current class *)
    fun getCurrentClass (E{myClass=SOME cls, ...}) = cls
      | getCurrentClass _ = raise Fail "getCurrentClass: no current class set"

  (* set the return type for the current method *)
    fun setReturnType (E{span, vEnv, myClass, info, ...}, retTyp) = E{
            span = span, vEnv = vEnv, myClass = myClass, retTyp = retTyp, info = info
          }

  (* get the return type for the current method *)
    fun getReturnType (E{retTyp, ...}) = retTyp

    fun insertClass (E{info=Info{clsEnv, ...}, ...}, id, cls) = ATbl.insert clsEnv (id, cls)

    fun insertIface (E{info=Info{ifcEnv, ...}, ...}, id, ifc) = ATbl.insert ifcEnv (id, ifc)

    fun findTyc (E{info=Info{clsEnv, ifcEnv, ...}, ...}, id) = (
          case (ATbl.find clsEnv id, ATbl.find ifcEnv id)
           of (SOME cls, NONE) => SOME(Types.ClsTy cls)
            | (NONE, SOME ifc) => SOME(Types.IfcTy ifc)
            | (NONE, NONE) => NONE
            | _ => raise Fail("internal error: two bindings for " ^ Atom.toString id)
          (* end case *))

    fun findClass (E{info=Info{clsEnv, ...}, ...}, id) = ATbl.find clsEnv id

    fun lookupClass (E{info=Info{clsEnv, ...}, ...}, id) = ATbl.lookup clsEnv id

    fun lookupIface (E{info=Info{ifcEnv, ...}, ...}, id) = ATbl.lookup ifcEnv id

  (* insert a local variable binding into the environment *)
    fun insertVar (E{span, vEnv, myClass, retTyp, info}, x, x') = E{
            span = span, vEnv = AMap.insert (vEnv, x, x'),
            myClass = myClass, retTyp = retTyp, info = info
          }

  (* lookup a variable; return NONE if it isn't bound *)
    fun findVar (E{vEnv, ...}, x) = AMap.find (vEnv, x)

  end
