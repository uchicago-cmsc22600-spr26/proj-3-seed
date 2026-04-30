(* util.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure Util : sig

  (* sort a list of member variables into canonical order *)
    val sortMemberVars : MemberVar.t list -> MemberVar.t list

  (* sort a list of member functions into canonical order *)
    val sortMemberFuns : MemberFun.t list -> MemberFun.t list

  end = struct

    structure Sort = ListMergeSort

    fun greater cmpFn (x, y) = (case cmpFn (x, y)
           of GREATER => true
            | _ => false
          (* end case *))

    val sortMemberFuns = Sort.sort (greater MemberFun.compare)

    val sortMemberVars = Sort.sort (greater MemberVar.compare)

  end
