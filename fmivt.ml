

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

type stime =  float * int

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
    time : stime;
    maxstep : float;
    interface :  interface_type
  }

and msg = 
  {
    sender : ustring;
    reciever : ustring;
    sentat : stime;
    receiveat : stime;
    sign : bool;
    data : (port * signal) option;
  }
   
and fmu = 
  {
    name : ustring;
    inputs : port list;
    outputs : port list;
    dependecies : (port * port) list; 
    state : f_state;
    lvt : stime;
    iqueue : (msg * bool) list;
    oqueue : msg list;
    squeue : (stime * f_state * (msg * bool) list * msg list) list
  }

and fmi =
  {
    fmuInstances : fmu list;
    allinputVar : port list;
    alloutputVar : port list;
    globalDependencies : ( port *  port) list;
    portMaping : (port * port) list;
    qdetails : (ustring * (msg * bool) list) list
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

let findMappingNotOpt (prt: port) (pMap: (port * port) list)  =
  List.assoc prt pMap
  
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


(*superdense time comparison*)

let gt (a1, a2) (b1, b2) =
   if (a1 = b1) then (if (a2 > b2) then true else false) else (if (a1 > b1) then true else false )

let lt (a1, a2) (b1, b2) =
     if (a1 = b1) then (if (a2 < b2) then true else false) else (if (a1 < b1) then true else false )

let strongsim (a1, a2) (b1, b2)  
  if ((a1 = b1) && (a2 = b2)) then true else false 

let weaksim (a1, a2) (b1, b2) = 
  if (a1 = b1) then true else false

let add (a1, a2) h = 
  if h = 0.0 then (a1, a2 + 1) else  (a1 +. h, a2)

let comparestime m1 m2 =
  let a = m1.lvt in
  let b = m2.lvt in
  if (strongsim a b) then 0 else (if (gt a b) then 1 else -1)

let sub (a1, a2) (b1, b2) = 
  let asub1 = abs_float (a1 -. b1) in 
  let asub2 = abs (a2 - b2) in
  if asub2 = 1 then 0.0 else asub1
  
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
 if (ipone <> 0.0 && opone <> 0.0) then
  (let count = res +. ipone +. iptwo in 
   let smap = {s with output_0 = Present(count); interface = SCounter(count);} in 
   (smap, h))
  else 
   (let smap = {s with output_0 = Absent(); interface = SCounter(res);} in 
   (smap, h))
   



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


let statediscreteclock step = {input_0 = Null(); input_1 = Null(); input_2 = Null(); input_3 = Null(); input_4 = Null();
	          output_0 = Absent(); output_1 = Null(); output_2 = Null(); output_3 = Null(); output_4 = Null(); maxstep = step; time = (0.0, 0); interface = SDiscrete()}

let statecounter = {input_0 = Null(); input_1 = Null(); input_2 = Null(); input_3 = Null(); input_4 = Null();
	          output_0 = Absent(); output_1 = Null(); output_2 = Null(); output_3 = Null(); output_4 = Null(); maxstep = infinity; time = (0.0, 0); interface = SCounter(0.0)}

let rec createAllMessage foutputs fstate lvt lmap =
  match foutputs with 
  |[] -> []
  |y1 :: rst -> let y_v = get fstate y1 in 
               let u1 = findMappingNotOpt y1 lmap in 
               let r1 = findFMUOfPort u1 in 
               let s1 = findFMUOfPort y1 in
               let m1 =  {sender = s1; reciever = r1; sentat = lvt; receiveat = add lvt 0.0; sign = true; data = Some (u1, y_v)} in
               m1 :: createAllMessage rst fstate lvt lmap  
 

let rec sendMessage ms fmuinst qmap = 
  match ms with 
  | mprime :: rst -> let oqueueprime =  ({mprime with sign=false}) :: fmuinst.oqueue in
                     let fmuprime = {fmuinst with oqueue = oqueueprime} in 
                     let qmapprime = List.map (fun a -> if (fst a) = mprime.reciever then (fst a,  (mprime, true) :: (snd a)) else a) qmap in
                     sendMessage rst fmuprime qmapprime
 | _ -> (qmap, fmuinst)

let rec sendAntiMessage ms fmuinst qmap = 
  match ms with 
  | mprime :: rst -> let qmapprime = List.map (fun a -> if (fst a) = mprime.reciever then (fst a,  (mprime, true) :: (snd a)) else a) qmap in
                     sendMessage rst fmuinst qmapprime
 | _ -> (qmap, fmuinst)


let rollbackFMU fmuinst m qmap smap = 
  let antimsgtosend = List.filter (fun a -> gt fmuinst.lvt a.sentat) fmuinst.oqueue in 
  let fmuprime = List.assoc (fmuinst.lvt) smap in
  let (qmap, fmuprime) = sendAntiMessage antimsgtosend fmuprime qmap in
  (fmuprime, qmap)
                   
 

let progressFMU fmuinst m qmap lmap =
  match m.data with  
  | Some(u, v) -> let fmustate = set fmuinst.state u v in
                  let fmuinst = {fmuinst with state = fmustate} in
                  let stepsize = getMaxStepSize fmuinst.state in 
                  let (sprime, hprime) =  (match stepsize with 
                           |infinity -> doStep fmustate infinity
                           |a -> (fmuinst.state, stepsize)) in
                  let mprime = {sender = fmuinst.name; reciever = fmuinst.name; sentat = fmuinst.lvt; receiveat = add fmuinst.lvt hprime; sign = true; data = None} in 
                  let (qmap, fmuprime) = sendMessage [mprime] fmuinst qmap in 
                  (fmuprime, qmap)
  | None       -> let (sprime, hprime) = doStep fmuinst.state (sub m.sentat m.receiveat) in
                  let fmuinst = {fmuinst with state = sprime} in
                  let allmsg = createAllMessage fmuinst.outputs fmuinst.state fmuinst.lvt lmap in 
                  let (qmap, fmuprime) = sendMessage allmsg fmuinst qmap in
                  (fmuprime, qmap)
                   
      
let updateLVTAntimessage fmuinst m qmap smap  =  
  if ((lt m.receiveat fmuinst.lvt)) then (rollbackFMU fmuinst m qmap smap) else (fmuinst, qmap)

let updateLVTMessage fmuinst m qmap lmap smap = 
  let localtime = fmuinst.lvt in 
  if ((gt m.receiveat fmuinst.lvt) || (strongsim  m.receiveat fmuinst.lvt)) then (progressFMU fmuinst m qmap lmap) else (rollbackFMU fmuinst m qmap smap) 

let nextMessage fmuinst = try Some (List.find (fun a -> (snd a) = true) fmuinst.iqueue) 
		          with Not_found -> None

let fmuvt fmuinst qmap smap lmap =
  let nm = nextMessage fmuinst in 
  match nm with
   |Some(m, true) -> let (fprime, qmapprime) = if (m.sign) then (updateLVTMessage fmuinst m qmap lmap smap) else (updateLVTAntimessage fmuinst m qmap smap) in
                      let iq = List.assoc fmuinst.name qmapprime in  
  		      let iqprime = List.map (fun a -> if ((strongsim m.receiveat (fst a).receiveat) && (m.sender = (fst a).sender)) then (fst a, false) else a) iq in
                      let qp = List.map (fun a -> if ((fst a) = fmuinst.name) then (fst a, iqprime) else a ) qmapprime in 
                      let iqprimefmu  = List.map (fun a -> if ((strongsim m.receiveat (fst a).receiveat) && (m.sender = (fst a).sender)) then (fst a, false) else a) fmuinst.iqueue in
                      ({fprime with iqueue=iqprimefmu}, qp, (fprime.lvt, fprime))
   |None -> let (fprime, qprime) = ({fmuinst with lvt = (infinity, 0)}, qmap) in  (fprime, qprime, (fprime.lvt, fprime))

   
    
 
(*Test function*)
let testSimpleCounter =
  let fmu1 = {name = us  "discreteclock1"; inputs = []; outputs = [Output_0(us "discreteclock1")]; dependecies = []; state = statediscreteclock 2.0; lvt = (infinity, 0); iqueue = []; oqueue = []; squeue = []} in 
  let fmu2 = {name =us  "discreteclock2"; inputs = []; outputs = [Output_0(us "discreteclock2")]; dependecies = []; state = statediscreteclock 5.0; lvt = (infinity, 0); iqueue = []; oqueue = []; squeue = [] } in 
  let fmu3 = {name = us "counter"; inputs = [Input_0(us "counter") ; Input_1(us "counter")]; outputs = [Output_0(us "counter")]; dependecies = [(Input_0(us "c"), Output_1(us "e")); (Input_1(us "d"), Output_0(us "e"))]; state = statecounter; lvt = (infinity, 0); iqueue = []; oqueue = []; squeue = []} in
  let iVar = List.append fmu1.inputs (List.append fmu2.inputs fmu3.inputs) in
  let oVar = List.append fmu1.outputs (List.append fmu2.outputs fmu3.outputs) in
  let pMap = [(Input_0(us "counter"), Output_0(us "discreteclock1")); (Input_1(us "counter"), Output_0(us "discreteclock2"))] in
  let lmap =  [(Output_0(us "discreteclock1"), Input_0(us "counter")); (Output_0(us "discreteclock2"), Input_1(us "counter"))] in
  let dep =  [(Input_0(us "counter"), Output_0(us "counter")); (Input_1(us "counter"), Output_0(us "counter"))] in
  let fmisimplecounter = {fmuInstances = [fmu1; fmu2; fmu3]; allinputVar = iVar; alloutputVar = oVar; globalDependencies = dep; portMaping = pMap; qdetails = [(fmu1.name, fmu1.iqueue); (fmu2.name, fmu2.iqueue); (fmu3.name, fmu3.iqueue)]} in
  let (cp, cr, cl) = ([], [fmu1; fmu2; fmu3], []) in
  let qMap = [(fmu1.name, []); (fmu2.name, []); (fmu3.name, [])] in
  let g = createGraph (dep) (pMap) (iVar) (oVar) in
  let topo = resTopologicalSort g in 
  printGraph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); printAllNodes topo; ()

 

 
