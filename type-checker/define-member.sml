(* define-member.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * Code to check the definitions of class and interface members in the
 * second pass of type checking.
 *)

structure DefineMember : sig

  (* An extended environment that tracks member functions and variables *)
    type env

  (* create an environment for checking class member-variable and function
   * declarations.  Pass in the initial lists of inherited variables and
   * functions.
   *)
    val newEnvForClass : Env.t * MemberVar.t list * MemberFun.t list -> env

  (* create an environment for checking interface member-function
   * definitions.  Pass in the initial list of inherited functions.
   *)
    val newEnvForInterface : Env.t * MemberFun.t list -> env

  (* get the list of member variables from the environment *)
    val varsOf : env -> MemberVar.t list

  (* get the list of member functions from the environment *)
    val funsOf : env -> MemberFun.t list

  (* check the signature of a member-variable declaration, which involves checking
   * for duplicate definitions and that the declared type is well-formed.
   *)
    val varDcl : ParseTree.var_dcl * env -> env

  (* check the signature of a member-function declaration, which involves checking
   * for duplicate definitions and proper overriding, and that the declared signature
   * of the function is well-formed.
   *)
    val funDcl : ParseTree.meth_dcl * env -> env

  (* check a member-function specification, which involves checking for duplicate
   * definitions and that the declared signature of the function is well formed.
   *)
    val funSpc : ParseTree.meth_spc * env -> env

  end = struct

    structure PT = ParseTree
    structure MVar = MemberVar
    structure MFun = MemberFun
    structure Err = TypeError

  (* import the token datatype to get access to the constructors without qualification *)
    datatype token = datatype Err.token

  (* sets of member variables; these are keyed by the variable
   * names (the types are ignored)
   *)
    structure VarSet = RedBlackSetFn (
      struct
        type ord_key = MVar.t
        val compare = MVar.compare
      end)
  (* finite maps of member functions; these are keyed by the function
   * names (the types are ignored)
   *)
    structure FunMap = RedBlackMapFn (
      struct
        type ord_key = MFun.t
        val compare = MFun.compare
      end)

  (* an extended environment *)
    datatype env = Env of {
        env : Env.t,                    (* the environment being extended *)
        vars : VarSet.set,              (* member variables *)
        funs : MFun.t option FunMap.map (* member functions; inherited functions map
                                         * to `SOME mfun`, where mfun is the inherited
                                         * function signature, while locally-defined
                                         * functions map to NONE.
                                         *)
      }

    fun newEnv (env, vars, funs) = Env{
            env = env,
            vars = VarSet.fromList vars,
            funs = List.foldl (fn (f, m) => FunMap.insert(m, f, SOME f)) FunMap.empty funs
          }

    val newEnvForClass = newEnv

    fun newEnvForInterface (env, funs) = newEnv (env, [], funs)

    fun varsOf (Env{vars, ...}) = VarSet.toList vars

    fun funsOf (Env{funs, ...}) = FunMap.listKeys funs

    fun setSpan (Env{env, funs, vars}, span) =
          Env{env = Env.setSpan(env, span), funs = funs, vars = vars}

    fun varDcl (PT.VDCL_Mark m, xenv) = varDcl (#tree m, setSpan(xenv, #span m))
      | varDcl (PT.VDCL(name, typ, _), xenv as Env{env, funs, vars}) = let
        (* check the declared type *)
          val typ' = CheckType.check (env, typ)
        (* define the variable *)
          val mvar = MVar.new(#tree name, typ')
          in
            if VarSet.member(vars, mvar)
              then (
                Err.error (env, [
                    S "redeclaration of member variable ", A(#tree name)
                  ]);
                xenv)
              else Env{env=env, funs=funs, vars=VarSet.add(vars, mvar)}
          end

    fun funDcl (PT.MDCL_Mark m, xenv) = funDcl (#tree m, setSpan(xenv, #span m))
      | funDcl (PT.MDCL{override, name, params, retTy, ...}, xenv as Env{env, funs, vars}) = let
        (* check the types *)
          val paramTys' = CheckParams.check (env, params)
          val retTy' = CheckType.checkReturnType (env, retTy)
        (* define the function *)
          val mfun = MFun.new(#tree name, paramTys', retTy')
          val funs' = FunMap.insert(funs, mfun, NONE)
        (* redeclaration error message *)
          fun redeclErr () = (
                Err.error (env, [S "redeclaration of member function ", A(#tree name)]);
                xenv)
          in
            if override
              then (case FunMap.find(funs, mfun)
                 of SOME(SOME mfun') => let
                      val Types.FUNTY(paramTys'', retTy'') = MFun.typeOf mfun'
                      in
                        if ListPair.allEq TypeUtil.equal (paramTys', paramTys'')
                        andalso TypeUtil.equal (retTy', retTy'')
                          then () (* okay *)
                          else Err.error (env, [
                              S "type mismatch in override of member-function ",
                              A(#tree name), S ".\n",
                              S "  expected: ", TYS paramTys'', S " -> ", TY retTy'', S "\n",
                              S "  found:    ", TYS paramTys',  S " -> ", TY retTy'
                            ]);
                        xenv
                      end
                  | SOME NONE => redeclErr ()
                  | NONE => (
                      Err.error (env, [
                          S "override of non-existent member function ", A(#tree name)
                        ]);
                    (* we add the function to environment anyway *)
                      Env{env=env, funs=funs', vars=vars})
                (* end case *))
              else (case FunMap.find(funs, mfun)
                 of SOME(SOME _) => (
                      Err.error (env, [
                          S "missing 'override' in redeclaration of inherited function ",
                          A(#tree name)
                        ]);
                      xenv)
                  | SOME NONE => redeclErr ()
                  | NONE => Env{env=env, funs=funs', vars=vars}
                (* end case *))
          end

    fun funSpc (PT.MSPC_Mark m, xenv) = funSpc (#tree m, setSpan(xenv, #span m))
      | funSpc (PT.MSPC{name, paramTys, retTy}, xenv as Env{env, funs, vars}) = let
        (* check the types *)
          val paramTys' = List.map (fn typ => CheckType.check (env, typ)) paramTys
          val retTy' = CheckType.checkReturnType (env, retTy)
        (* define the function *)
          val mfun = MFun.new(#tree name, paramTys', retTy')
          in
            if FunMap.inDomain(funs, mfun)
              then (
                Err.error (env, [
                    S "redefinition of member function ", A(#tree name)
                  ]);
                xenv)
              else Env{env=env, funs=FunMap.insert(funs, mfun, NONE), vars=vars}
          end

  end
