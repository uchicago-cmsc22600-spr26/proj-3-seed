(* check-params.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure CheckParams : sig

  (* type check a list of parameters: check for duplicate names and the
   * well-formedness of the declared types.  This function returns the
   * type signature of the parameters.
   *)
    val check : Env.t * ParseTree.param list -> Types.t list

  (* given a list of parameters and a list of their types (from check),
   * create local variables for the parameters and return a pair of
   * the list of local variables and the environment enriched with
   * the parameter bindings.
   *)
    val bindParams : Env.t * ParseTree.param list * Types.t list
          -> Env.t * LocalVar.t list

  end = struct

    structure PT = ParseTree
    structure Err = TypeError
    structure ASet = AtomSet

  (* import the token datatype to get access to the constructors without qualification *)
    datatype token = datatype Err.token

    fun check (env, params) = let
          fun chk (PT.PARAM_Mark m, (env, xs, tys)) =
                chk (#tree m, (Env.setSpan(env, #span m), xs, tys))
            | chk (PT.PARAM({span, tree}, typ), (env, xs, tys)) = let
                val typ = CheckType.check (env, typ)
                in
                  if ASet.member(xs, tree)
                    then (
                      Err.error (Env.setSpan(env, span), [
                          S "duplicate parameter name ", A tree
                        ]);
                      (env, xs, typ::tys))
                    else (env, ASet.add(xs, tree), typ::tys)
                end
          val (_, _, tys) = List.foldl chk (env, ASet.empty, []) params
          in
          (* reverse types to get left-to-right order *)
            List.rev tys
          end

    fun bindParams (env, params, tys) = let
        (* Note that we do not track spans, since there are no error messages, and we
         * do not worry about duplicates here, since we have alread reported any
         * duplicate parameter errors in the `check` function.
         *)
          fun bind (PT.PARAM_Mark m, typ, (env, xs)) = bind (#tree m, typ, (env, xs))
            | bind (PT.PARAM({tree, ...}, _), typ, (env, xs)) = let
                val x = LocalVar.new (tree, typ)
                in
                  (Env.insertVar (env, tree, x), x::xs)
                end
          in
          (* process from right to left so the result order is correct and so that the
           * first binding takes precedence when there are duplicates.
           *)
            ListPair.foldrEq bind (env, []) (params, tys)
          end

  end
