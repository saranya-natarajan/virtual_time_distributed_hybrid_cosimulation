open Ustring.Op
open List
open Fmi
open Components


(*Find all edges and create graph *)

let find_fmu_of_port fmulist portvar =  
  List.find (fun b -> List.exists (fun a -> a = portvar) (List.append b.fmuinputs b.fmuoutputs)) fmulist

let find_mapping (prt: port) (pmap: (port * port) list)  =
  try Some (List.assoc prt pmap) 
  with Not_found -> None

let find_state_of_port portvar smap fmulist =
   let fmuofport =  find_fmu_of_port fmulist portvar in
   List.find (fun a -> fmuofport.debugname  = (fst a)) smap

let all_fmus_with_preditable_step_size allfmus =
  let (cp, cr, cl) = allfmus in 
  cp

let all_fmus_with_rollback allfmus =
  let (cp, cr, cl)  = allfmus in 
  cr

let all_legacy_fmus allfmus =
  let (cp, cr, cl)  = allfmus in 
  cl

let rec save_state cr smap = 
  match cr with 
  |ht :: rst -> let st = List.assoc ht.debugname smap in
                (ht.debugname, st) :: (save_state rst smap)
  |[] -> []

let rec restore_state cr rmap = 
  match cr with 
  | ht :: rst -> let st = List.assoc ht.debugname rmap in
			 (ht.debugname, st) :: restore_state rst rmap 
  | [] -> []
   
 
let rec find_all_edges (pmap :(port * port) list) (ivar : port list) = 
  match ivar with 
  | [] -> [] 
  | h :: rst -> match (find_mapping h pmap) with 
	       |Some a  -> (a, h) :: find_all_edges pmap rst 
               |None -> find_all_edges pmap rst 
                                    
let create_graph (dep :  (port * port) list) (pmap:  (port * port) list) (ivar :  port list) (ovar:port list) = 
  let g = {nodes =  List.append ivar ovar ;
	   edges =  List.append dep (find_all_edges pmap ivar)}  in
  g

(*Print functions *) 
let print_edge (a, b) = 
  (uprint_string  ( us "(")); (uprint_string a); (uprint_string  ( us ", ")); (uprint_string b); (uprint_string  ( us ")")); uprint_newline () 

let print_all_nodes nlist = 
 List.iter (fun a -> uprint_string a;uprint_newline () )  nlist; uprint_newline () 

let print_all_edges elist =  
  List.iter print_edge elist;  uprint_newline () 

let print_graph (g : graph) =
   uprint_string (us "NODES: "); uprint_newline (); print_all_nodes g.nodes; uprint_string (us "EDGES: ");uprint_newline (); print_all_edges g.edges 


(*topological search *)
let rec nodes_with_no_incoming_edge nodelist edgelist =
  match nodelist with 
  | h :: rst -> (match (List.exists (fun a -> (snd a = h) ) edgelist) with 
	         | true -> nodes_with_no_incoming_edge  rst edgelist
		 | false -> h ::  nodes_with_no_incoming_edge  rst edgelist)
  | [] -> []


let rec topological_sort noincominglist sortedlist edgelist g  =
  match noincominglist with
  | h :: rst -> let auxsortedlist = h :: sortedlist in
		let (nodeswithoutedges, nodeswithedges) =  List.split (List.filter (fun a -> (fst a = h)) edgelist) in
	        let auxedgelist = List.filter (fun a -> (snd a <> h)) edgelist in 
                let auxnoincominglist = nodes_with_no_incoming_edge nodeswithedges auxedgelist in
		let auxauxnoincominglist = (match auxnoincominglist with 
					    | [] -> List.append nodeswithedges rst 
 					    | _ -> rst) in 
	        topological_sort auxauxnoincominglist auxsortedlist auxedgelist g 
  | [] -> (sortedlist, edgelist)

let result_topological_sort g =
  let noincominglist = nodes_with_no_incoming_edge g.nodes g.edges in 
  let (sortlist, edgelist) = topological_sort noincominglist [] g.edges g in 
  (match edgelist with 
  | [] -> sortlist 
  | _ -> raise Not_found)

(*let get m y =
  let (nme, ste) = m in  
  let (var, value) = List.find (fun a -> (fst a) = y) ste.portvalue in 
  value
*)

let get m y =
  let (nme, s) = m in 
  match s.addvar with 
  |SCounter(count) -> get_counter s y 
  |SDiscrete(t, p) -> get_periodic_clock s y 
 
let set m u v =
 let (name, ste) = m in 
 let portupdated = List.map (fun a -> if (u = (fst a)) then ((fst a), v) else a) ste.portvalue in 
 let mupdated = (name, {ste with portvalue = portupdated}) in 
 mupdated

let do_step s h =
 match s.addvar with 
 |SCounter(a) -> do_step_counter s h
 |SDiscrete(t, p) -> do_step_periodic_clock s h


let get_max_step_size s = 
 match s.addvar with
 |SCounter(a) -> infinity 
 |SDiscrete(t, p) -> t

let rec min_step_size_of_fmu clist hp smap = 
  match clist with
  | ht :: rst -> let s = List.assoc ht.debugname smap in 
                 let h = get_max_step_size s in 
                 if (h < hp) then (min_step_size_of_fmu rst h smap) else (min_step_size_of_fmu rst hp smap)
  | [] -> hp

let rec do_step_list cr smap h =
  match cr with 
  | ht :: rst -> let s = List.assoc ht.debugname smap in 
                 let (sprime, hprime) = do_step s h in
                 let hmin = min h hprime in
                 let smp = List.map (fun a -> if (ht.debugname = (fst a)) then (ht.debugname, sprime) else a) smap in 
		 do_step_list rst smp hmin 
  | [] -> (smap, h)
  

let rec step_one orderedinputs smap pmap fmulist =
  match orderedinputs with 
  | u :: rst -> let yopt = find_mapping u pmap in 
                (match yopt with 
                 |Some(y) -> let my = find_state_of_port y smap fmulist in 
                             let v = get my y in 
                             let mu = find_state_of_port  u smap fmulist in
                             let muupdated = set mu u v in 
                             let smapupdated = List.map (fun a -> if (fst a = (fst muupdated)) then (muupdated) else a) smap in 
			     step_one rst  smapupdated pmap fmulist  
                 |None  -> step_one rst smap pmap fmulist)
 |[] -> smap

let step_two allfmus smap hmax = 
  let cp = all_fmus_with_preditable_step_size allfmus in
  let h = min_step_size_of_fmu cp hmax smap in 
  h

let step_three allfmus smap =
  let cr = all_fmus_with_rollback allfmus in
  let r = save_state cr smap in
  r 
 
let step_four allfmus smap =
  let cr = all_fmus_with_rollback allfmus in
  let smapprime = restore_state cr smap in 
  smapprime 

let step_five allfmus smap hmin =
  let cr = all_fmus_with_rollback allfmus in
  let (smapprime, h)  = do_step_list cr smap hmin in
  (smapprime, h)
  
let rec rollback_on_accepted_step allfmus smap r h = 
  let (smapprime, hprime) =  step_five allfmus smap h in 
  let hmin = min hprime h in 
  if (hmin < h) then (let smaprestored = step_four allfmus r in rollback_on_accepted_step allfmus smaprestored r hmin) else (smapprime, hprime)

let rec step_eight allfmus smap h = 
  let cp = all_fmus_with_preditable_step_size allfmus in 
  let (smapprime, hprime) = do_step_list cp smap h in 
  (smapprime, hprime)


let output_of_model modelname smap =
  if modelname = (us "simplestcounter") then 
    let my = List.find (fun a -> (fst a) = (us "counter")) smap in
    let y = us "c" in
    let (name, ste) = my in
    let v =  get my y in 
    uprint_string (us "count="); print_signal v;  uprint_newline ()

let masterStepSuperdenseTime fmiset allfmus orderedlist pmap hmax smap =
  (*step one and check*)
  (*check : List.iter (fun a -> print_state (fst a) (snd a)) smap ; *)
  let orderedinputs = List.filter (fun a -> List.exists (fun b -> b = a) fmiset.allinputvar) orderedlist in
  let smap = step_one orderedinputs smap pmap fmiset.fmuinstances in
  let _ = output_of_model fmiset.fminame smap in 
  (*check : let _ = List.iter (fun a -> print_state (fst a) (snd a)) smap in  *)
  let h = step_two allfmus smap hmax in 
  let r = step_three allfmus smap in 
  let (m, h) = rollback_on_accepted_step allfmus smap r h in (*step 4 to step 6*)
  let (mprime, hprime) = step_eight allfmus m h in (* skipping step 7 since no legacy supported in this implementation *)
  if h <> hprime then raise Not_found else (mprime, hprime) 

   
let initialize_state_periodic_clock portvar period = 
   {portvalue = [(portvar, Absent())]; time = {time.model_time = 0.0; time.index =0}; index_counter = 0; addvar = SDiscrete(0.0, period)}
   
let initialize_state_counter inputvar outputvar initcount= 
   {portvalue = [(inputvar, Absent()); (outputvar, Absent())]; time = {time.model_time = 0.0; time.index =0}; index_counter = 0; addvar = SCounter(initcount)}


let rec runSimulation fmiinstance allfmus orderedlist pmap m startime endtime timenow h =  
  let (mprime, hprime) = masterStepSuperdenseTime fmiinstance allfmus orderedlist pmap h m in 
   let _ = uprint_string (us "h = "); uprint_float h; uprint_newline() in
  let _ = assert(hprime>=0.0) in
  let _ = uprint_string (us "accepted h = "); uprint_float hprime; uprint_newline() in 
  let _ =  uprint_newline () in
(*  let (modeltime, index) = timenow in *)
  let timenow = if (hprime = 0.0) then {timenow with index = (timenow.index + 1)} else {timenow with model_time = (model_time +. hprime); index = 0} in     
  let _ = print_time timenow in
  if (fst timenow.model_time <= endtime.model_time) then runSimulation fmiinstance allfmus orderedlist pmap mprime startime endtime timenow 2.0 else () 
   
let testSimplestCounter =
  let fmu1 = {fmuinputs = []; fmuoutputs = [us "a"]; fmudependecies =[]; fmustate = initialize_state_periodic_clock (us "a") 3.0; debugname = (us "periodic_clock0")} in
  let fmu2 = {fmuinputs = [us "b"]; fmuoutputs = [us "c"]; fmudependecies =[(us "b", us "c")]; fmustate = initialize_state_counter (us "b") (us "c") 0.0; debugname = (us "counter")} in
  let ivar = List.append fmu1.fmuinputs fmu2.fmuinputs in 
  let ovar = List.append fmu1.fmuoutputs fmu2.fmuoutputs in
  let pmap = [((us "b"), (us "a"))] in 
  let dep = List.append fmu1.fmudependecies fmu2.fmudependecies in 
  let fmisimplecounter = {fmuinstances = [fmu1; fmu2]; allinputvar = ivar; alloutputvar = ovar; globaldependencies = dep; portmapping = pmap; fminame = us "simplestcounter"} in
  let allfmus = ([fmu1;fmu2], [], []) in 
  let smap = [(fmu1.debugname, fmu1.fmustate); (fmu2.debugname, fmu2.fmustate)] in  
  let g = create_graph (dep) (pmap) (ivar) (ovar) in
  let start_time = {model_time = 0.0; index = 0} in 
  let end_time = {model_tim = 21.0; index = 0} in 
  let topo = List.rev (result_topological_sort g) in 
  print_graph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); print_all_nodes topo;
  runSimulation (fmisimplecounter) allfmus (topo) pmap smap start_time end_time end_time 2.0; () 



