(* normalize.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *
 * The translation from the SooL AST to SOIR.
 *)

structure Normalize : sig

    val transform : AST.program -> SOIR.program

  end = struct

    structure Ty = Types
    structure MVar = MemberVar
    structure MFun = MemberFun
    structure IR = SOIR

  (* convert an AST local variable to a SOIR variable and add it to the environment *)
    fun cvtVar (env, x) = let
          val x' = Util.newVar x
          in
            (Env.bindVar(env, x, x'), x')
          end

  (* lookup a member-function for class in the environment returning the SOIR.item for
   * the function.  This item specifies the function's offset it the class's metadata.
   *)
    fun getItemForFunInCls (env, cls, mfun) = raise Fail "YOUR CODE HERE"

  (* lookup an index table for ifc' in the index table for ifc *)
    fun getItemForIfcInTbl (env, ifc, ifc') = raise Fail "YOUR CODE HERE"

  (* lookup a member function in the index table for an interface *)
    fun getItemForFunInTbl (env, ifc, mfun) = raise Fail "YOUR CODE HERE"

  (* get the initialization function for class in the environment *)
    fun getInitFunc (env, cls) = raise Fail "YOUR CODE HERE"

  (* create a new SOIR local variable *)
    val freshVar = SOIRVar.new

  (* add a SOIR local-variable declaration and pass the resulting value to `scope` *)
    fun withVar (name, ty, e, scope : IR.value -> IR.stm list) = let
          val x = freshVar (name, ty)
          in
            IR.LocalVarStm(x, SOME e) :: scope(IR.VarVal x)
          end

  (* add a SOIR new statement and pass the resulting value to `scope` *)
    fun newStm (name, cls, scope : IR.value -> IR.stm list) = let
          val x = freshVar (name, IR.ObjectTy)
          in
            IR.NewStm(x, cls) :: scope(IR.VarVal x)
          end

  (* add a SOIR local-variable binding to a tuple and pass the resulting value to `scope` *)
    fun tupleStm (name, ty, vs, scope : IR.value -> IR.stm list) = let
          val x = freshVar (name, ty)
          in
            IR.TupleStm(x, vs) :: scope(IR.VarVal x)
          end

  (* add a SOIR apply statement and pass the resulting value to `scope` *)
    fun applyStm (name, ty, f, args, scope : IR.value -> IR.stm list) = let
          val x = freshVar (name, ty)
          in
            IR.ApplyStm(x, f, args) :: scope(IR.VarVal x)
          end

  (* type of wrapped object *)
    val wrappedObjTy = IR.TupleTy[IR.ObjectTy, IR.IndexTy]

    fun ifThen (v, b) = IR.IfStm(v, b, NONE)

    fun ifThenElse (v, b1, b2) = IR.IfStm(v, b1, SOME b2)

  (* make an IR block *)
    fun mkBlock stms = IR.Block{lab = ProgPt.new(), stms = stms}

  (* a block of IR code that signals a runtime failure *)
    fun mkFail msg = mkBlock[
            IR.CallStm(IR.FuncVal SOIRBasis.failFunc, [IR.StringVal msg])
          ]

  (* member variable for accessing an object's metadata field *)
    val mdField = IR.MVAR{name = "_md", ty = IR.ClassTy, offset = 0}

  (* convert a (non-equality) binary operator to a SOIR primop *)
    fun cvtPrim rator = (case rator
           of BinOp.^<^ => PrimOp.IntLt
            | BinOp.^<=^ => PrimOp.IntLte
            | BinOp.^==^ => raise Fail "unexpected '=='"
            | BinOp.^!=^ => raise Fail "unexpected '!='"
            | BinOp.^@^ => raise Fail "unexpected '@'"
            | BinOp.^+^ => PrimOp.IntAdd
            | BinOp.^-^ => PrimOp.IntSub
            | BinOp.^*^ => PrimOp.IntMul
            | BinOp.^/^ => PrimOp.IntDiv
          (* end case *))

  (* convert an AST block to a SOIR block; if the `needsRet` flag is true,
   * then we need to add a return at the end of the block (if there isn't
   * one already)
   *)
    fun blockToBlock (env, needsRet, AST.BLK stms) = mkBlock (stmsToStms (env, needsRet, stms))

  (* convert a list of AST statements to a list of SOIR statements *)
    and stmsToStms (_, true, []) = [IR.ReturnStm NONE]
      | stmsToStms (_, false, []) = []
      | stmsToStms (env, needsRet, stm::stms) = raise Fail "YOUR CODE HERE"

  (* `nilTest (env, e, ifNil, ifNotNil, k)`
   * normalizes code that needs to test the expression `e` for "nil"
   *)
    and nilTest (env, exp, ifNil : unit -> IR.block, ifNotNil : IR.value -> IR.block option, k) =
          expToValue (env, exp, fn v =>
            withVar ("isNil", IR.BoolTy, IR.PrimExp(PrimOp.isNil, [v]), fn isNil =>
              IR.IfStm(isNil, ifNil(), ifNotNil v) ::
              k v))

  (* normalize an AST expression to a SOIR expression *)
    and expToExp (env, exp, k : IR.exp -> IR.stm list) = (case exp
           of AST.EXP_PrimOp(e1, BinOp.^@^, e2) =>
                expToValue (env, e1, fn v1 =>
                  expToValue (env, e2, fn v2 =>
                    applyStm ("strRes", IR.StringTy, IR.FuncVal SOIRBasis.stringCatFunc, [v1, v2],
                      fn res => k(IR.ValExp res))))
            | AST.EXP_PrimOp(e1, rator, e2) => if BinOp.isEqOp rator
                then raise Fail "YOUR CODE HERE"
                else expToValue (env, e1, fn v1 =>
                  expToValue (env, e2, fn v2 =>
                    k (IR.PrimExp(cvtPrim rator, [v1, v2]))))
            | AST.EXP_Select{obj, cls, opt=false, var} =>
                expToValue (env, obj, fn obj' => let
                  val mvar = Env.lookupMVar (env, cls, var)
                  in
                    k (IR.MemberVarExp(obj', mvar))
                  end)
            | AST.EXP_Coerce{from, to, e} =>
                expToValue (env, e, coerceValue (env, from, to, fn v => k(IR.ValExp v)))
            | _ => (* no direct conversion to a SOIR expression, so convert to a value instead *)
                expToValue (env, exp, fn v => k (IR.ValExp v))
          (* end case *))

  (* normalize an expression to SOIR code that defines a value, which is
   * passed to the context 'k'
   *)
    and expToValue (env, exp, k : IR.value -> IR.stm list) =
          raise Fail "YOUR CODE HERE"

  (* normalize a list of expressions to code that defines a list of values, which are
   * passed to the context 'k'
   *)
    and expsToValues (env, exps, k) = let
          fun toValues ([], vs) = k (List.rev vs)
            | toValues (e::es, vs) = expToValue (env, e, fn v => toValues (es, v::vs))
          in
            toValues (exps, [])
          end

  (* normalize an AST.mfun_call to a SOIR expression *)
    and funCallToIR (env, {obj, objTy, opt, f, args}, k : IR.exp -> IR.stm list) =
          raise Fail "YOUR CODE HERE"

  (* coerce the value `v` and from type `from` to type `to` and pass to context `k` *)
    and coerceValue (env, from, to, k : IR.value -> IR.stm list) v =
          raise Fail "YOUR CODE HERE"

  (* generate the SooL main function, which executes the SooL statement
   *
   *    main().run();
   *)
    fun genSoolMain (env, mainCls) = let
          val obj = SOIRVar.new ("obj", IR.ObjectTy)
          val initFn = getInitFunc (env, mainCls)
          val md = SOIRVar.new ("md", IR.ClassTy)
          val runItem = getItemForFunInCls (env, mainCls, Basis.runFun)
          val run = SOIRVar.new ("run", IR.FuncTy([IR.ObjectTy], IR.VoidTy))
          in
            IR.FuncDef{
                f = SOIRBasis.soolMain, params = [],
                body = mkBlock[
                    IR.NewStm(obj, Env.lookupClass(env, mainCls)),
                    IR.CallStm(IR.FuncVal initFn, [IR.VarVal obj]),
                    IR.LocalVarStm(md, SOME(IR.MemberVarExp(IR.VarVal obj, mdField))),
                    IR.LocalVarStm(run, SOME(IR.MetaSel(IR.VarVal md, runItem))),
                    IR.CallStm(IR.VarVal run, [IR.VarVal obj]),
                    IR.ReturnStm NONE
                  ]
              }
          end

    fun transform (prog as AST.PROGRAM{main, dcls}) = let
          val info = AnalyzeAST.analyze prog
        (* make the top-level environment *)
          val topEnv = Env.new info
(* YOUR CODE HERE *)
          in
            IR.PROGRAM{
                classes = #classes info,
                indices = #indices info,
                functions = [],(* YOUR CODE HERE *)
                main = genSoolMain (topEnv, main)
              }
          end

  end
