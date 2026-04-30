(* typechecker.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * This is the root module for the type checker.  It handles processing the top-level
 * class and interface declarations.  To support recursive definitions, typechecking
 * is broken down into three passes:
 *
 * 1.  insert placeholder bindings for each top-level class and interface declaration
 *     This pass also checks 'extends' clauses for a prior definition of the extended
 *     class or interface.
 *
 * 2.  fill in the placeholder bindings by checking the declared types of member variables
 *     and functions.
 *
 * 3.  typecheck initialization expressions and member-function bodies.  This checking
 *     includes checking against
 *)

structure TypeChecker : sig

    val check : Error.err_stream * ParseTree.program -> AST.program

  end = struct

    structure PT = ParseTree
    structure Ty = Types
    structure MVar = MemberVar
    structure MFun = MemberFun
    structure DefMemb = DefineMember
    structure Err = TypeError

  (* import the token datatype to get access to the constructors without qualification *)
    datatype token = datatype Err.token

    val selfAtom = Atom.atom "self"

  (* the first pass walks over the top-level definitions and records a mapping
   * from identifiers to class/interface types.  It also checks that super classes
   * have been defined and that classes and interfaces are not redeclared.
   *)
    fun bindTopDcls (env, dcls) = let
          fun bindTopDcl (env, PT.DCL_Mark m) =
                bindTopDcl (Env.setSpan(env, #span m), #tree m)
            | bindTopDcl (env, PT.DCL_Class{name={tree=clsId, ...}, super, ...}) = (
                case Env.findTyc (env, clsId)
                 of SOME _ => Err.error (env, [S "redeclaration of class ", A clsId])
                  | NONE => let
                    (* check that if there is a super class that it is defined *)
                      val super = (case super
                             of SOME({span, tree}, _) => (case Env.findTyc (env, tree)
                                   of SOME(Ty.ClsTy cls) => SOME cls
                                    | SOME _ => (
                                        Err.error (Env.setSpan(env, span), [
                                            S "class ", A clsId,
                                            S " cannot inherit from interface ", A tree
                                          ]);
                                        NONE)
                                    | _ => (
                                        Err.error (Env.setSpan(env, span), [
                                            S "undefined super class ", A tree,
                                            S " in definition of class ", A clsId
                                          ]);
                                        NONE)
                                  (* end case *))
                              | NONE => NONE
                            (* end case *))
                      val cls = Class.new (clsId, super)
                      in
                      (* add the class to the environment *)
                        Env.insertClass (env, clsId, cls)
                      end
                (* end case *))
            | bindTopDcl (env, PT.DCL_Interface{name={tree=ifcId, ...}, extends, ...}) = (
                case Env.findTyc (env, ifcId)
                 of SOME _ => Err.error (env, [S "redeclaration of interface ", A ifcId])
                  | NONE => let
                    (* check that if this definition is an extension, that
                     * the extension is defined
                     *)
                      val extends = (case extends
                             of SOME{span, tree} => (case Env.findTyc (env, tree)
                                   of SOME(Ty.IfcTy ifc) => SOME ifc
                                    | SOME _ => (
                                        Err.error (Env.setSpan(env, span), [
                                            S "interface ", A ifcId,
                                            S " cannot extend from class ", A tree
                                          ]);
                                        NONE)
                                    | _ => (
                                        Err.error (Env.setSpan(env, span), [
                                            S "undefined interface ", A tree,
                                            S " in definition of interface ", A ifcId
                                          ]);
                                        NONE)
                                  (* end case *))
                              | NONE => NONE
                            (* end case *))
                      in
                      (* add the interface to the environment *)
                        Env.insertIface (env, ifcId, Interface.new ifcId)
                      end
                (* end case *))
          in
            List.app (fn dcl => bindTopDcl(env, dcl)) dcls;
          (* we quit if there were any errors *)
            if Env.anyErrors env then raise Error.ERROR else ()
          end

  (* define a class from its definition.  This process requires the following actions:
   *   - check that params are unique and get the type signature of the params
   *   - if there is a super class, get its members
   *   - check the well-formedness of the variable and method types
   *   - check for duplicate member definitions (including super class members)
   *   - check that overriding member functions are redefined at the same type
   *     as in the superclass
   *)
    fun defineClass (env, {name={tree=clsName, span}, params, super, vars, meths}) = let
        (* get the class tycon that we defined in pass one *)
          val cls = Env.lookupClass (env, clsName)
        (* check the parameter types *)
          val paramTys = CheckParams.check (env, params)
        (* get the inherited member functions and variables (if any) *)
          val (membVars, membFuns) = (case Class.super cls
                 of NONE => ([], [])
                  | SOME superCls => (Class.varsOf superCls, Class.funsOf superCls)
                (* end case *))
        (* process the variable and function declarations *)
          val xenv = DefMemb.newEnvForClass (env, membVars, membFuns)
          val xenv = List.foldl DefMemb.varDcl xenv vars
          val xenv = List.foldl DefMemb.funDcl xenv meths
          in
          (* fill in the definition of the interface type constructor *)
            Class.init (cls, paramTys, DefMemb.varsOf xenv, DefMemb.funsOf xenv)
          end

  (* define an interface from its definition.  This process requires the following actions:
   *   - if the interface extends another, get the other interface's members
   *   - check the well-formedness of the method types
   *   - check for duplicate member definitions
   *)
    fun defineInterface (env, {name={tree=ifcName, span}, extends, meths}) = let
        (* get the interface tycon that we defined in pass one *)
          val ifc = Env.lookupIface (env, ifcName)
        (* get the member functions from the parent interface (if any) *)
          val membFuns = (case extends
                 of NONE => []
                  | SOME{tree=ifc', span} => Interface.funsOf(Env.lookupIface (env, ifc'))
                (* end case *))
        (* process the function specs *)
          val xenv = DefMemb.newEnvForInterface (env, membFuns)
          val xenv = List.foldl DefMemb.funSpc xenv meths
          in
          (* fill in the definition of the interface type constructor *)
            Interface.init (ifc, DefMemb.funsOf xenv)
          end

  (* the second pass walks over the declarations and fills in the placeholder class
   * and interface constructors that were inserted into the environment in the
   * first pass.
   *)
    fun defineTopDcls (env, dcls) = let
          fun defineTopDcl (env, PT.DCL_Mark m) =
                defineTopDcl (Env.setSpan(env, #span m), #tree m)
            | defineTopDcl (env, PT.DCL_Class cls) = defineClass (env, cls)
            | defineTopDcl (env, PT.DCL_Interface ifc) = defineInterface (env, ifc)
          in
            List.app (fn dcl => defineTopDcl(env, dcl)) dcls;
          (* we quit if there were any errors *)
            if Env.anyErrors env then raise Error.ERROR else ()
          end

  (* typecheck a class definition. This process requires the following actions:
   *   - if there is a super class, check that its initialization has the correct type
   *   - the rhs of variable initializations are a subtype of the variable's type
   *   - check method definitions
   *)
    fun chkClass (env, {name={span, tree}, params, super, vars, meths}) = let
        (* get the class tycon that we defined in pass one *)
          val cls = Env.lookupClass (env, tree)
        (* set the current class to support visibility checking *)
          val env = Env.setCurrentClass (env, cls)
        (* bind the parameters and extend the environment *)
          val (env', params') = let
                val paramTys = Class.paramTypesOf cls
                in
                  CheckParams.bindParams (env, params, paramTys)
                end
        (* check the super-class initialization (if any) *)
          val super' = (case (Class.super cls, super)
                 of (SOME superCls, SOME(_, args)) => let
                      val paramTys = Class.paramTypesOf superCls
                      val args' = CheckExp.checkArgs (env', paramTys, args)
                      in
                        SOME(superCls, args')
                      end
                  | _ => NONE
                (* end case *))
        (* check the variable declarations in the environment extended with the parameters *)
          val vars' = let
                fun chkVar (env, PT.VDCL_Mark m) = chkVar (Env.setSpan(env, #span m), #tree m)
                  | chkVar (env, PT.VDCL(x, typ, rhs)) = let
                      val SOME x' = Class.findVar (cls, #tree x)
                      val rhs' = CheckExp.expectTy(env, MVar.typeOf x', rhs)
                      in
                        AST.VDCL(x', rhs')
                      end
                in
                  List.map (fn dcl => chkVar (env', dcl)) vars
                end
        (* for methods with non-void return type, we need to check that control will
         * always reach a return statement.  We do this check by analyzing the AST,
         * since it is simplier than the parse-tree representation.
         *)
          fun mustReturn (AST.BLK stms) = let
                fun stmMustRet (AST.STM_Return _) = true
                  | stmMustRet (AST.STM_If(conds, SOME blk)) =
                      mustReturn blk andalso List.all (fn (_, blk) => mustReturn blk) conds
                  | stmMustRet _ = false
                in
                  List.exists stmMustRet stms
                end
        (* check the fucntion declarations in the environment extended with "self" *)
          val funs' = let
                fun chkFun (env, PT.MDCL_Mark m) = chkFun (Env.setSpan(env, #span m), #tree m)
                  | chkFun (env, PT.MDCL{override, name, params, retTy, body}) = let
                      val SOME f' = Class.findFun (cls, #tree name)
                      val Ty.FUNTY(paramTys, retTy) = MFun.typeOf f'
                      val (env, params') = CheckParams.bindParams (env, params, paramTys)
                      val self = LocalVar.new (selfAtom, Ty.ClsTy cls)
                      val env = Env.insertVar (env, selfAtom, self)
                      val body = CheckBlock.check (Env.setReturnType (env, retTy), body)
                      in
                        case retTy
                         of AST.VoidTy => ()
                          | _ => if mustReturn body
                              then ()
                              else Err.error (env, [
                                  S "control may reach end of member function ",
                                  A(#tree name), S " with non-void return type"
                                ])
                        (* end case *);
                        AST.FDCL{
                            override = override, name = f', self = self,
                            params = params', retTy = MFun.returnTyOf f',
                            body = body
                          }
                      end
                in
                  List.map (fn dcl => chkFun (env, dcl)) meths
                end
          in
            AST.DCL_Class{
                tyc = cls, params = params', super = super',
                vars = vars', funs = funs'
              }
          end

  (* the third pass typechecks the top-level class declarations and builds the AST *)
    fun checkTopDcls (env, dcls) = let
          fun chkTopDcl (env, PT.DCL_Mark m, dcls') =
                chkTopDcl (Env.setSpan(env, #span m), #tree m, dcls')
            | chkTopDcl (env, PT.DCL_Class cls, dcls') = chkClass (env, cls) :: dcls'
            | chkTopDcl (env, PT.DCL_Interface{name, ...}, dcls') =
                AST.DCL_Interface(Env.lookupIface(env, #tree name)) :: dcls'
          in
            List.foldr (fn (dcl, dcls) => chkTopDcl(env, dcl, dcls)) [] dcls
          end

    fun check (errStrm, PT.PROGRAM{span, tree=dcls}) = let
          val env = Env.new (errStrm, span)
        (* first we get the top-level class and interface bindings *)
          val _ = bindTopDcls (env, dcls)
        (* now we can check the top-level types and set their definitions *)
          val _ = defineTopDcls (env, dcls)
        (* the last step is to check the method bodies, etc. and convert the class definitions
         * to abstract syntax (note that interfaces are filtered out of the AST)
         *)
          val dcls' = checkTopDcls (env, dcls)
        (* check for main class *)
          val main = (case Env.findTyc (env, Atom.atom "main")
                 of SOME(Ty.ClsTy mainCls) => if TypeUtil.implements(mainCls, Basis.mainI)
                      then mainCls (* okay *)
                      else (
                        Err.error (env, [
                            S "'main' class does not implement 'run' function with correct type"
                          ]);
                        mainCls)
                  | _ => (
                        Err.error (env, [
                            S "no 'main' class defined"
                          ]);
                        Class.new(Atom.atom "bogus-main", NONE))
                (* end case *))
          in
            AST.PROGRAM{main = main, dcls = dcls'}
          end

  end
