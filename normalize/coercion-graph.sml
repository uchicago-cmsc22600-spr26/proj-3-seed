(* coercion-graph.sml
 *
 * COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
 * All rights reserved.
 *
 * Sample code
 * CMSC 22600
 * Spring 2026
 * University of Chicago
 *)

structure CoercionGraph : sig

    type t

    type node

  (* construct a graph from a set of coercions.  There will be a node for each
   * class and interface mentioned in the set and an edge for each coercion.
   * We assume that there are coercions for every subclass to parent class
   * inherits relation in the program.
   *)
    val new : Coercions.Set.set -> t

  (* return the list of roots of the graph (i.e., nodes with in-degree 0) *)
    val rootsOf : t -> node list

  (* return the list of outgoing edges in a node *)
    val edgesOf : node -> node list

  (* compute the closure of the interfaces in the graph *)
    val computeClosure : t -> unit

  (* return the interfaces of a class; these will be in sorted order *)
    val interfacesOfClass : t -> Class.t -> Interface.t list

  (* return the interfaces of an interface; these will be in sorted order *)
    val interfacesOfInterface : t -> Interface.t -> Interface.t list

  (* the representation of the inheritance tree with edges going from classes to
   * their subclasses.
   *)
    datatype inherits_tree = InhNd of Class.t * inherits_tree list

  (* extract the inheritance tree for a base class from the coercion graph *)
    val getInheritanceTree : t * Class.t -> inherits_tree

  (* `output (outS, showIfcs, graph)`
   * print `graph` to `outS`; if `showIfcs` is true, include the interfaces attached
   * to each node in the output.
   *)
    val output : TextIO.outstream * bool * t -> unit

  end = struct

    structure C = Coercions
    structure CSet = Coercions.Set
    structure Ty = Types
    structure CTbl = Class.Tbl
    structure ITbl = Interface.Tbl
    structure ISet = Interface.Set

    datatype t = G of {
        nodes : node list,                      (* list of all of the nodes in the graph *)
        clsMap : Class.t -> node option,        (* mapping from classes to their nodes *)
        ifcMap : Interface.t -> node option     (* mapping from interfaces to their nodes *)
      }

    and node = Nd of {
        label : cls_or_ifc,                     (* the node's label *)
        ifaces : ISet.set ref,                  (* the interfaces attached to a node *)
        inDeg : int ref,                        (* number of incoming edges *)
        out : node list ref                     (* the output edges of a node *)
      }

    and cls_or_ifc = Cls of Class.t | Ifc of Interface.t

  (* create new nodes *)
    local
      fun newNode tag tyc = Nd{
              label = tag tyc,
              ifaces = ref ISet.empty,
              inDeg = ref 0,
              out = ref []
            }
    in
    val newClsNode = newNode Cls
    val newIfcNode = newNode Ifc
    end (* local *)

  (* does a node an a zero in degree? *)
    fun isRoot (Nd{inDeg, ...}) = (!inDeg = 0)

  (* increment a node's in degree *)
    fun incDegree (Nd{inDeg, ...}) = inDeg := !inDeg + 1

  (* decrement a node's in degree and return true if it goes to zero *)
    fun decDegree (Nd{inDeg as ref n, ...}) = let
          val n = n-1
          in
            inDeg := n;  (n = 0)
          end

    fun new cset = let
          val cTbl = CTbl.mkTable(CSet.numItems cset, Fail "class tbl")
          val iTbl = ITbl.mkTable(CSet.numItems cset, Fail "interface tbl")
          val findCls = CTbl.find cTbl
          val findIfc = ITbl.find iTbl
        (* generic function for getting/creating a node *)
          fun nodeOf (new, find, insert) tyc = (case find tyc
                 of NONE => let
                      val nd = new tyc
                      in
                        insert (tyc, nd);
                        nd
                      end
                  | SOME nd => nd
                (* end case *))
        (* specialized for classes and interfaces *)
          val clsNode = nodeOf (newClsNode, findCls, CTbl.insert cTbl)
          val ifcNode = nodeOf (newIfcNode, findIfc, ITbl.insert iTbl)
        (* add nodes and edges for coercions *)
          fun addEdge coercion = let
                fun add (Nd{out, ...}, dst) = (
                      out := dst :: !out;
                      incDegree dst)
                in
                  case coercion
                   of C.ClsCls(cls1, cls2) => add (clsNode cls1, clsNode cls2)
                    | C.IfcCls(ifc, cls) => add (ifcNode ifc, clsNode cls)
                    | C.IfcIfc(ifc1, ifc2) => add (ifcNode ifc1, ifcNode ifc2)
                  (* end case *)
                end
          in
          (* add nodes and edges for the coercions in the set *)
            CSet.app addEdge cset;
          (* build the graph *)
            G{  nodes = CTbl.listItems cTbl @ ITbl.listItems iTbl,
                clsMap = findCls,
                ifcMap = findIfc
              }
          end

  (* return the list of roots of the graph (i.e., nodes with in-degree 0) *)
    fun rootsOf (G{nodes, ...}) = List.filter isRoot nodes

  (* return the list of outgoing edges in a node *)
    fun edgesOf (Nd{out, ...}) = !out

  (* return the interfaces of a node *)
    fun interfacesOf (Nd{ifaces, ...}) = !ifaces

  (* `addInterfaces ifcs nd`
   * adds the set of interfaces `ifcs` to the node `nd`
   *)
    fun addInterfaces ifcs (Nd{ifaces, ...}) = ifaces := ISet.union(!ifaces, ifcs)

  (* compute the closure of the interfaces in the graph.  We walk the graph
   * by visiting nodes once all of their predecesors have been processed.
   * We implement this traversal by decrementing a node's in-degree and adding
   * it to the work list when the in degree reaches zero.
   *)
    fun computeClosure cg = let
        (* decrement the in degree of nodes on a list and add them to the work list
         * if their in degree is 0.
         *)
          fun addNodes (q, []) = q
            | addNodes (q, nd::nds) = if decDegree nd
                then addNodes(nd::q, nds)
                else addNodes(q, nds)
          fun walk [] = ()
            | walk (nd::nds) = let
                val ifcs = interfacesOf nd
                val ifcs = (case nd of Nd{label=Ifc ifc, ...} => ISet.add(ifcs, ifc) | _ => ifcs)
                val succs = edgesOf nd
                in
                (* propagate interfaces to the successors of nd *)
                  List.app (addInterfaces ifcs) succs;
                (* add new roots to the work list and continue *)
                  walk (addNodes (nds, succs))
                end
          in
            walk (rootsOf cg)
          end

  (* return the interfaces of a class *)
    fun interfacesOfClass (G{clsMap, ...}) cls = (case clsMap cls
           of NONE => []
            | SOME nd => ISet.toList(interfacesOf nd)
          (* end case *))

  (* return the interfaces of an interface *)
    fun interfacesOfInterface (G{ifcMap, ...}) ifc = (case ifcMap ifc
           of NONE => []
            | SOME nd => ISet.toList(interfacesOf nd)
          (* end case *))

  (* the representation of the inheritance tree with edges going from classes to
   * their subclasses.
   *)
    datatype inherits_tree = InhNd of Class.t * inherits_tree list

  (* extract the inheritance tree for a base class from the coercion graph *)
    fun getInheritanceTree (G{clsMap, ...}, cls) = let
          fun getTree cls = (case clsMap cls
                 of NONE => InhNd(cls, [])
                  | SOME nd => let
                      fun doEdge (Nd{label=Cls cls, ...}, subclasses) =
                            getTree cls :: subclasses
                        | doEdge (_, subclasses) = subclasses
                      in
                        InhNd(cls, List.foldl doEdge [] (edgesOf nd))
                      end
                (* end case *))
          in
            getTree cls
          end

  (* print the graph to the given output file. We use "dot" syntax (graphviz.org),
   * in case we want to visualize the graph.
   * Note that the "root" annotation is inaccurate once the closure has been computed.
   *)
    fun output (outS, showIfcs, G{nodes, ...}) = let
          fun pr s = TextIO.output(outS, String.concat s)
          fun nodeToString (Nd{label, ...}) = (case label
                 of Cls cls => Class.nameOf cls
                  | Ifc ifc => Interface.nameOf ifc
                (* end case *))
          fun info (Nd{label, ifaces, inDeg, ...}) = let
                val s = (case label of Cls _ => "class" | _ => "interface")
                val s = if (!inDeg = 0) then s ^ ", root" else s
                in
                  if showIfcs
                    then concat[
                        s, ", {",
                        String.concatWithMap "," Interface.nameOf (ISet.toList (!ifaces)),
                        "}"
                      ]
                    else s
                end
          fun prNode nd = (case edgesOf nd
                 of [] => pr ["  ", nodeToString nd, "; /* ", info nd, " */\n"]
                  | nds => (
                      pr ["  ", nodeToString nd, " /* ", info nd, " */\n    -> {"];
                      List.app (fn nd => pr [" ", nodeToString nd]) nds;
                      pr [" };\n"])
                (* end case *))
          in
            pr ["digraph {\n"];
              List.app prNode nodes;
            pr ["}\n"]
          end

  end
