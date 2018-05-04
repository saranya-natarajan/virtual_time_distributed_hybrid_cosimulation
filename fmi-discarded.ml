

open Ustring.Op
open List


exception FMU_error of string
(* port : name
   Note : this implementation supports only real values *)


type port_type = 
  |Input_0  of ustring 
  |Input_1  of ustring 
  |Input_2  of ustring 
  |Input_3  of ustring 
  |Input_4  of ustring 
  |Output_0 of ustring 
  |Output_1 of ustring 
  |Output_2 of ustring 
  |Output_3 of ustring 
  |Output_4 of ustring 
   
type port = port_type

type signal = 
  |Present of float 
  |Absent  of unit
  |Null of unit
 

and interface_type = 
  |SCounter of float 
  |SDiscrete of unit

and f_state = 
  {
    input_0  : signal;
    input_1  : signal;
    input_2  : signal;
    input_3  : signal;
    input_4  : signal;
    output_0 : signal;
    output_1 : signal;
    output_2 : signal;
    output_3 : signal;
    output_4 : signal;
    time : int * int;
    maxstep : float;
    interface :  interface_type 
  }
   
and fmu = 
  {
    name : ustring;
    inputs : port list;
    outputs : port list;
    dependecies : (port * port) list; 
    state : f_state;
  }

and fmi =
  {
    fmuInstances : fmu list;
    allinputVar : port list;
    alloutputVar : port list;
    globalDependencies : ( port *  port) list;
    portMaping : (port * port) list;
  }

and graph = 
  {
    nodes : port list;
    edges : (port * port) list;
  }

(*Find all edges and create graph *)

let findMapping (prt: port) (pMap: (port * port) list)  =
  try Some (List.assoc prt pMap) 
  with Not_found -> None

  
let rec findAllEdge (pMap : (port * port) list) (iVar : port list) = 
  match iVar with 
  | [] -> [] 
  | h :: rst -> match (findMapping h pMap) with 
	       |Some a  -> (a, h) :: findAllEdge pMap rst 
               |None -> findAllEdge pMap rst 
                                    
let createGraph (dep :  (port * port) list) (pMap:  (port * port) list) (iVar :  port list) (oVar:port list) = 
  let g = {nodes =  List.append iVar oVar ;
	   edges =  List.append dep (findAllEdge pMap iVar)}  in
  g

(*Print functions *) 
let printNode nde =  
  match nde with 
  |Input_0(a) -> uprint_string ((a ^. us "-input0" )) 
  |Input_1(a) -> uprint_string ((a ^. us "-input1" )) 
  |Input_2(a) -> uprint_string ((a ^. us "-input2" ))  
  |Input_3(a) -> uprint_string ((a ^. us "-input3" ))  
  |Input_4(a) -> uprint_string ((a ^. us "-input4" )) 
  |Output_0(a) -> uprint_string ((a ^. us "-output0" ))  
  |Output_1(a) -> uprint_string ((a ^. us "-output1" ))  
  |Output_2(a) -> uprint_string ((a ^. us "-output2" )) 
  |Output_3(a) -> uprint_string ((a ^. us "-output3"  )) 
  |Output_4(a) -> uprint_string ((a ^. us "-output4" ))  

let printEdge (a, b) = 
  (uprint_string  ( us "(")); (printNode a); (uprint_string  ( us ", ")); (printNode b); (uprint_string  ( us ")")); uprint_newline () 

let printAllNodes nlist = 
 List.iter (fun a -> printNode a;uprint_newline () )  nlist; uprint_newline () 

let printAllEdges elist =  
  List.iter printEdge elist;  uprint_newline () 

let printGraph (g : graph) =
   uprint_string (us "NODES: "); uprint_newline (); printAllNodes g.nodes; uprint_string (us "EDGES: ");uprint_newline (); printAllEdges g.edges 

let printStateFieldValue fval  =  
  match fval with 
  |Present(vl) -> uprint_float (vl); uprint_newline (); 
  |Absent() -> uprint_string (us "ABSENT"); uprint_newline (); 
  |Null() -> ()

let isPort prt = 
  match prt with 
  |Present(_) -> true 
  |Absent() -> true 
  |Null() -> false  

let printState fname s = 
    uprint_string ((us "STATE OF FMU ") ^. (fname)); uprint_newline ();
    if (isPort s.input_0) then (uprint_string (fname ^. us "-"  ^. us "input0 = "); printStateFieldValue s.input_0);
    if (isPort s.input_1) then (uprint_string (fname ^. us "-"  ^. us "input1 = "); printStateFieldValue s.input_1);
    if (isPort s.input_2) then (uprint_string (fname ^. us "-"  ^. us "input2 = "); printStateFieldValue s.input_2);
    if (isPort s.input_3) then (uprint_string (fname ^. us "-"  ^. us "input3 = "); printStateFieldValue s.input_3);
    if (isPort s.input_4) then (uprint_string (fname ^. us "-"  ^. us "input4 = "); printStateFieldValue s.input_4);
    if (isPort s.output_0) then (uprint_string (fname ^. us "-"  ^. us "output0 = "); printStateFieldValue s.output_0);
    if (isPort s.output_1) then (uprint_string (fname ^. us "-"  ^. us "output1 = "); printStateFieldValue s.output_1);
    if (isPort s.output_2) then (uprint_string (fname ^. us "-"  ^. us "output2 = "); printStateFieldValue s.output_2);
    if (isPort s.output_3) then (uprint_string (fname ^. us "-"  ^. us "output3 = "); printStateFieldValue s.output_3);
    if (isPort s.output_4) then (uprint_string (fname ^. us "-"  ^. us "output4 = "); printStateFieldValue s.output_4); 
    uprint_newline (); ()


(*topological search *)
let rec findAllNodesWithNoIncomingEdge nodeList edgeList =
  match nodeList with 
  | h :: rst -> (match (List.exists (fun a -> (snd a = h) ) edgeList) with 
	         | true -> findAllNodesWithNoIncomingEdge rst edgeList
		 | false -> h ::  findAllNodesWithNoIncomingEdge rst edgeList)
  | [] -> []


let rec topologicalsort noincominglist sortedlist edgelist g  =
  match noincominglist with
  | h :: rst -> let auxsortedlist = h :: sortedlist in
		let (nodeswithoutedges, nodeswithedges) =  List.split (List.filter (fun a -> (fst a = h)) edgelist) in
	        let auxedgelist = List.filter (fun a -> (snd a <> h)) edgelist in 
                let auxnoincominglist = findAllNodesWithNoIncomingEdge nodeswithedges auxedgelist in
		let auxauxnoincominglist = (match auxnoincominglist with 
					    | [] -> List.append nodeswithedges rst 
 					    | _ -> rst) in 
	        topologicalsort auxauxnoincominglist auxsortedlist auxedgelist g 
  | [] -> (sortedlist, edgelist)

let resTopologicalSort g =
  let noincominglist = findAllNodesWithNoIncomingEdge g.nodes g.edges in 
  let (sortlist, edgelist) = topologicalsort noincominglist [] g.edges g in 
  (match edgelist with 
  | [] -> sortlist 
  | _ -> raise Not_found)

let counter_solver s h res =
  let ipone = (match s.input_0 with 
                  |Present(value) -> value
                  |Absent() -> 0.0
                  |Null() -> raise (FMU_error "input 0 of counter is NULL")) in  
  let iptwo = (match s.input_1 with 
                  |Present(value) -> value
                  |Absent() -> 0.0
                  |Null() -> raise (FMU_error "input 2 of counter is NULL")) in 
  let count = res +. ipone +. iptwo in 
  let smap = {s with output_0 = Present(count); interface = SCounter(count);} in 
   (smap, h)
   



let discrete_solver s h =
  (s, h) 
  
  

let findStateMapOfPort prt smap = 
  match prt with 
  |Input_0(a)  -> List.assoc a smap 
  |Input_1(a)  -> List.assoc a smap 
  |Input_2(a)  -> List.assoc a smap 
  |Input_3(a)  -> List.assoc a smap
  |Input_4(a)  -> List.assoc a smap 
  |Output_0(a) -> List.assoc a smap 
  |Output_1(a) -> List.assoc a smap 
  |Output_2(a) -> List.assoc a smap 
  |Output_3(a) -> List.assoc a smap 
  |Output_4(a) -> List.assoc a smap 

let findFMUOfPort prt = 
  match prt with 
  |Input_0(a)  -> a 
  |Input_1(a)  -> a
  |Input_2(a)  -> a
  |Input_3(a)  -> a
  |Input_4(a)  -> a
  |Output_0(a) -> a
  |Output_1(a) -> a
  |Output_2(a) -> a
  |Output_3(a) -> a 
  |Output_4(a) -> a 

let findStateOfFMU fname sMap = 
  List.assoc fname sMap 

let get m_y y = 
  match y with 
  |Output_0(a) -> m_y.output_0 
  |Output_1(a) -> m_y.output_1 
  |Output_2(a) -> m_y.output_2 
  |Output_3(a) -> m_y.output_3 
  |Output_4(a) -> m_y.output_4 
  |_ -> raise Not_found

let set m_u u v = 
  match u with 
  |Input_0(a) -> {m_u with input_0 = v} 
  |Input_1(a) -> {m_u with input_1 = v}
  |Input_2(a) -> {m_u with input_2 = v}
  |Input_3(a) -> {m_u with input_3 = v}
  |Input_4(a) -> {m_u with input_4 = v}
  |_ -> raise Not_found

let getMaxStepSize s = 
  s.maxstep 

let doStep s h =
  match s.interface with 
  |SCounter(count)  -> counter_solver s h count 
  |SDiscrete() -> discrete_solver s h  

 

let rec minStepSizeOfFMU cp hp smap =
  match cp with 
  | ht :: rst -> let s = findStateOfFMU ht.name smap in 
                 let h = getMaxStepSize s in 
		 if (h < hp) then (minStepSizeOfFMU rst h smap) else (minStepSizeOfFMU rst hp smap) 
  | [] -> hp 


let allFMUWithPredictableStepSize allfmu = 
  match allfmu with 
  |(cp, cr, cl) -> cp 
  | _ -> raise Not_found 

let allFMUWithRollBack allfmu = 
  match allfmu with 
  |(cp, cr, cl) -> cr
  | _ -> raise Not_found

let rec saveState cr smap =  
  match cr with 
  | ht :: rst -> let s = List.assoc ht.name smap in 
                 (ht.name, s) :: saveState rst smap 
  | [] -> [] 

let rec restoreState cr rmap = 
  match cr with 
  | ht :: rst -> let s = List.assoc ht.name rmap in
			 (ht.name, s) :: restoreState rst rmap 
  | [] -> []

let rec doStepOnFMU cr smap h =
  match cr with 
  | ht :: rst -> let s = List.assoc ht.name smap in 
                 let (sprime, hprime) = doStep s h in
                 let hmin = min h hprime in
                 let smp = List.map (fun a -> if (ht.name = (fst a)) then (ht.name, sprime) else a) smap in 
		 doStepOnFMU rst smp hmin 
  | [] -> (smap, h)


let init_state step intrfce = {input_0 = Absent(); input_1 = Absent(); input_2 = Absent(); input_3 = Absent(); input_4 = Absent();
	          output_0 = Present(5.0); output_1 = Absent(); output_2 = Absent(); output_3 = Absent(); output_4 = Absent(); maxstep = step; time = (0, 0); interface = intrfce}


let rec step_one orderedlist m pmap =
  match orderedlist with 
  | u :: rst -> let yopt = (match u with 
                 	    |Input_0(a) as iport -> findMapping iport pmap
                            |Input_1(a) as iport -> findMapping iport pmap
                	    |Input_2(a) as iport -> findMapping iport pmap
                            |Input_3(a) as iport -> findMapping iport pmap
                            |Input_4(a) as iport -> findMapping iport pmap
                            |_ -> None )in 
 		 (match yopt with 
                 |Some(y) -> let m_y = findStateMapOfPort y m in
                             let v = get m_y y in
                             let m_u = findStateMapOfPort u m in
		             let m_u_new = set m_u u v in
                             let smap = List.map (fun a -> if (fst a = findFMUOfPort u) then (fst a, m_u_new) else (fst a, m_u)) m in 
			     step_one rst smap pmap 
                 |None  -> step_one rst m pmap )
 | [] -> m

let step_two allfmus smap hmax = 
  let cp = allFMUWithPredictableStepSize allfmus in
  let h =  minStepSizeOfFMU cp hmax smap in
  h 

let step_three allfmus smap =
  let cr = allFMUWithRollBack allfmus in
  let r = saveState cr smap in
  r 
 
let step_four allfmus smap =
  let cr = allFMUWithRollBack allfmus in
  let smapprime = restoreState cr smap in 
  smapprime 

let step_five allfmus smap hmin =
  let cr = allFMUWithRollBack allfmus in
  let (smapprime, h)  = doStepOnFMU cr smap hmin in
  (smapprime, h)
  
let rec rollbackOnAcceptedStep allfmus smap r h = 
  let (smapprime, hprime) =  step_five allfmus smap h in 
  let hmin = min hprime h in 
  if (hmin < h) then (let smaprestored = step_four allfmus r in rollbackOnAcceptedStep allfmus smaprestored r hmin) else (smap, h)

let rec step_seven allfmus smap h = 
  let cp = allFMUWithRollBack allfmus in 
  let (smapprime, hprime) = doStepOnFMU cp smap h in 
  (smapprime, hprime)


let masterStepSuperdenseTime allfmus orderedlist pmap hmax m =
  (*step one and check*)
  List.iter (fun a -> printState (fst a) (snd a)) m ; 
  let smap = step_one orderedlist m pmap in 
  List.iter (fun a -> printState (fst a) (snd a) ) smap ; 
  (*step two and check*)
  let h = step_two allfmus smap hmax in
  let _ = uprint_float h in 
  (*step two and check*)
  let r = step_three allfmus smap in
  (*step 4 to step 6 on repeat*)
  let (m, h) =  rollbackOnAcceptedStep allfmus smap r h in 
  let (mprime, hprime) = step_seven allfmus m h in
  if h <> hprime then raise Not_found else (mprime, hprime) 




 
(*Test function*)
let testSimpleCounter =
  let fmu1 = {name = us  "discreteclock1"; inputs = []; outputs = [Output_0(us "discreteclock1")]; dependecies = []; state = init_state infinity (SDiscrete())} in 
  let fmu2 = {name =us  "discreteclock2"; inputs = []; outputs = [Output_0(us "discreteclock2")]; dependecies = [];  state = init_state 5. (SDiscrete()) } in 
  let fmu3 = {name = us "counter"; inputs = [Input_0(us "counter") ; Input_1(us "counter")]; outputs = [Output_0(us "counter")]; dependecies = [(Input_0(us "c"), Output_1(us "e")); (Input_1(us "d"), Output_0(us "e"))]; state = init_state 3. (SCounter(0.0))} in
  let iVar = List.append fmu1.inputs (List.append fmu2.inputs fmu3.inputs) in
  let oVar = List.append fmu1.outputs (List.append fmu2.outputs fmu3.outputs) in
  let pMap = [(Input_0(us "counter"), Output_0(us "discreteclock1")); (Input_1(us "counter"), Output_0(us "discreteclock2"))] in 
  let dep =  [(Input_0(us "counter"), Output_0(us "counter")); (Input_1(us "counter"), Output_0(us "counter"))] in
  let fmisimplecounter = {fmuInstances = [fmu1; fmu2; fmu3]; allinputVar = iVar; alloutputVar = oVar; globalDependencies = dep; portMaping = pMap} in
  let (cp, cr, cl) = ([], [fmu1; fmu2; fmu3], []) in
  let sMap = [(fmu1.name, fmu1.state); (fmu2.name, fmu2.state); (fmu3.name, fmu3.state)] in
  let g = createGraph (dep) (pMap) (iVar) (oVar) in
  let topo = resTopologicalSort g in 
  printGraph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); printAllNodes topo;
  masterStepSuperdenseTime  (cp, cr, cl) topo pMap 10.0 sMap; () 

 

 
