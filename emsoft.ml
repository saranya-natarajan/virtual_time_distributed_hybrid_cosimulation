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

let rec add_other_fmu_with_no_rollback cp smap = 
  match cp with 
  | ht :: rst -> let st = List.assoc ht.debugname smap in
			 (ht.debugname, st) :: restore_state rst smap 
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
  let _ = uprint_string ((us "get: ") ^. nme ^. (us ":")) in  
  (match s.addvar with 
  |SCounter(count) -> get_counter s y 
  |SAdder(sum) -> get_adder s y
  |SDiscrete(t, p) -> get_periodic_clock s y
  |SConst(a) -> get_const s y
  |SGain(_, _) -> get_gain s y
  |SDisreteTimeDelay(_,_)-> get_discrete_time_delay s y
  |SMicrostepDelay(_) -> get_microstep_delay s y) 
 
let set m u v =
 let (name, ste) = m in 
 let _ = uprint_string ((us "set ") ^. name ^. (us ":")) in  
 let portupdated = List.map (fun a -> if (u = (fst a)) then ((fst a), v) else a) ste.portvalue in
 let _ = print_port (List.find (fun a -> u = (fst a)) portupdated); uprint_newline() in
 let mupdated = (name, {ste with portvalue = portupdated}) in 
 mupdated

let do_step s h =
 match s.addvar with 
 |SCounter(a) -> do_step_counter s h
 |SAdder(a) -> do_step_adder s h
 |SDiscrete(t, p) -> do_step_periodic_clock s h
 |SConst(a) -> do_step_const s h
 |SGain(_, _) -> do_step_gain s h 
 |SDisreteTimeDelay(_,_) -> do_step_discrete_time_delay s h
 |SMicrostepDelay(_) -> do_step_microstep_delay  s h  


let get_max_step_size s = 
 match s.addvar with
 |SCounter(a) -> get_max_step_size_counter s 
 |SAdder(a) -> get_max_step_size_adder s 
 |SDiscrete(t, p) -> get_max_step_size_periodic_clock s
 |SConst(a) -> get_max_step_size_const s
 |SDisreteTimeDelay(_,_) -> get_max_step_size_discrete_time_delay s 
 |SMicrostepDelay(_) -> get_max_step_size_microstep_delay s  
 |_ -> uprint_string (us "fmu does not support predictable step size"); raise Not_found

let rec min_step_size_of_fmu clist hp smap = 
  match clist with
  | ht :: rst -> let s = List.assoc ht.debugname smap in 
                 let h = get_max_step_size s in 
                 if (h < hp) then (min_step_size_of_fmu rst h smap) else (min_step_size_of_fmu rst hp smap)
  | [] -> hp

let rec do_step_list cr smap h =
  match cr with 
  | ht :: rst -> (*let _ = uprint_string ht.debugname  in*)
                 let s = List.assoc ht.debugname smap in
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
 
let step_four allfmus r smap =
  let cr = all_fmus_with_rollback allfmus in
  let smapprime = restore_state cr r in
  let cp = all_fmus_with_preditable_step_size allfmus in
  let smapprimeprime = add_other_fmu_with_no_rollback cp smap in 
  List.append smapprime smapprimeprime 

let step_five allfmus smap hmin =
  let cr = all_fmus_with_rollback allfmus in
  let (smapprime, h)  = do_step_list cr smap hmin in
  (smapprime, h)
  
let rec rollback_on_accepted_step allfmus smap r h = 
  let (smapprime, hprime) =  step_five allfmus smap h in 
  let hmin = min hprime h in 
  if (hmin < h) then (let smaprestored = step_four allfmus r smap in rollback_on_accepted_step allfmus smaprestored r hmin) else (smapprime, hprime)

let rec step_eight allfmus smap h = 
  let cp = all_fmus_with_preditable_step_size allfmus in 
  let (smapprime, hprime) = do_step_list cp smap h in
  (smapprime, hprime)


let output_of_model modelname smap =
  if (modelname = us "simplestcounter") then
                     (let my = List.find (fun a -> (fst a) = (us "counter")) smap in
                      let y = us "c" in
                      let (name, ste) = my in
                      let _ = get my y in ());
  if (modelname = us "simplecounterplusadder") then 
                             (let my = List.find (fun a -> (fst a) = (us "adder0")) smap in 
                              let y = us "f" in
                              let (name, ste) = my in
                              let _ = get my y in () ); 
  if (modelname = us "gainplusperiodic") then 
                             (let my = List.find (fun a -> (fst a) = (us "counter")) smap in 
                              let y = us "e" in
                              let (name, ste) = my in
                              let _ = get my y in () );
  if (modelname = us "discretedelay") then 
                             (let my = List.find (fun a -> (fst a) = (us "counter")) smap in 
                              let y = us "c" in
                              let (name, ste) = my in
                              let _ = get my y  in
                              let my = List.find (fun a -> (fst a) = (us "discretedelay")) smap in 
                              let y = us "e" in
                              let (name, ste) = my in
                              let _ = get my y  in () ); 
  if (modelname = us "microstepdelay") then 
                             (let my = List.find (fun a -> (fst a) = (us "periodic_clock0")) smap in 
                              let y = us "a" in
                              let (name, ste) = my in
                              let _ = get my y  in
                              let my = List.find (fun a -> (fst a) = (us "microstepdelay")) smap in 
                              let y = us "c" in
                              let (name, ste) = my in
                              let _ = get my y  in () ); () 




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
  if h <> hprime then (uprint_string (us "communication step size error");raise Not_found )else (mprime, hprime) 


let rec runSimulation fmiinstance allfmus orderedlist pmap m startime endtime timenow h =  
  let _ = print_time timenow in
  let _ = uprint_string (us "h = "); uprint_float h; uprint_newline() in
  let (mprime, hprime) = masterStepSuperdenseTime fmiinstance allfmus orderedlist pmap h m in 
  let _ = assert(hprime>=0.0) in
  let _ = uprint_string (us "accepted h = "); uprint_float hprime; uprint_newline() in 
  let timenow = if (hprime = 0.0) then {timenow with index = (timenow.index + 1)} else {timenow with model_time = (timenow.model_time +. hprime); index = 0} in     
  let _ = print_time timenow in
  let _ = uprint_newline() in
  if (timenow.model_time <= endtime.model_time) then (runSimulation fmiinstance allfmus orderedlist pmap mprime startime endtime timenow 2.0) else () 
   
let testSimplestCounter =
  let _ = uprint_string (us "periodic_clock -- counter");uprint_newline() in
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
  let end_time = {model_time = 21.0; index = 0} in 
  let topo = List.rev (result_topological_sort g) in 
  print_graph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); print_all_nodes topo;
  runSimulation (fmisimplecounter) allfmus (topo) pmap smap start_time end_time start_time 2.0; () 


(*let testCounterAdder =
  let _ = uprint_string (us "periodic_clock -- counter -- const -- adder"); uprint_newline() in
  let fmu1 = {fmuinputs = []; fmuoutputs = [us "a"]; fmudependecies =[]; fmustate = initialize_state_periodic_clock (us "a") 3.0; debugname = (us "periodic_clock0")} in
  let fmu2 = {fmuinputs = [us "b"]; fmuoutputs = [us "c"]; fmudependecies =[(us "b", us "c")]; fmustate = initialize_state_counter (us "b") (us "c") 0.0; debugname = (us "counter0")} in
  let fmu3 = {fmuinputs = [us "d"; us "e"]; fmuoutputs = [us "f"]; fmudependecies =[(us "d", us "f"); (us "e", us "f")]; fmustate = initialize_state_adder [(us "d"); (us "e"); (us "f")] 0.0; debugname = (us "adder0")} in
  let fmu4 = {fmuinputs = []; fmuoutputs = [us "g"]; fmudependecies =[]; fmustate = initialize_state_const [(us "g")] 3.0; debugname = (us "const0")} in
  let ivar = List.append fmu1.fmuinputs (List.append fmu2.fmuinputs (List.append fmu3.fmuinputs fmu4.fmuinputs)) in 
  let ovar = List.append fmu1.fmuoutputs (List.append fmu2.fmuoutputs (List.append fmu3.fmuoutputs fmu4.fmuoutputs)) in
  let pmap = [((us "b"), (us "a")); ((us "d"), (us "c"));  ((us "e"), (us "g"))] in 
  let dep =  List.append fmu1.fmudependecies (List.append fmu2.fmudependecies (List.append fmu3.fmudependecies fmu4.fmudependecies)) in 
  let fmicounteradder = {fmuinstances = [fmu1; fmu2; fmu3; fmu4]; allinputvar = ivar; alloutputvar = ovar; globaldependencies = dep; portmapping = pmap; fminame = us "simplecounterplusadder"} in
  let allfmus = ([fmu1;fmu2; fmu3], [fmu4], []) in 
  let smap = [(fmu1.debugname, fmu1.fmustate); (fmu2.debugname, fmu2.fmustate); (fmu3.debugname, fmu3.fmustate); (fmu4.debugname, fmu4.fmustate)] in  
  let g = create_graph (dep) (pmap) (ivar) (ovar) in
  let start_time = {model_time = 0.0; index = 0} in 
  let end_time = {model_time = 0.0; index = 0} in 
  let topo = List.rev (result_topological_sort g) in 
  print_graph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); print_all_nodes topo;
  runSimulation (fmicounteradder) allfmus (topo) pmap smap start_time end_time start_time 2.0; () *)

let testCounterGain =
  let _ = uprint_string (us "periodic_clock -- gain"); uprint_newline() in
  let fmu1 = {fmuinputs = []; fmuoutputs = [us "a"]; fmudependecies =[]; fmustate = initialize_state_periodic_clock (us "a") 3.0; debugname = (us "periodic_clock0")} in
  let fmu2 = {fmuinputs = [us "b"]; fmuoutputs = [us "c"]; fmudependecies =[(us "b", us "c")]; fmustate = initialize_state_gain [(us "b"); (us "c")] 0.0; debugname = (us "gain0")} in
  let fmu3 = {fmuinputs = [us "d"]; fmuoutputs = [us "e"]; fmudependecies =[(us "d", us "e")]; fmustate = initialize_state_counter (us "d") (us "e") 3.0; debugname = (us "counter")} in
  let ivar = List.append fmu1.fmuinputs  (List.append fmu2.fmuinputs fmu3.fmuinputs) in 
  let ovar = List.append fmu1.fmuoutputs (List.append fmu2.fmuoutputs fmu3.fmuoutputs) in
  let pmap = [((us "b"), (us "a")); ((us "d"), (us "c"))] in 
  let dep =  List.append fmu1.fmudependecies (List.append fmu2.fmudependecies fmu3.fmudependecies)in 
  let fmicounteradder = {fmuinstances = [fmu1; fmu2; fmu3]; allinputvar = ivar; alloutputvar = ovar; globaldependencies = dep; portmapping = pmap; fminame = us "gainplusperiodic"} in
  let allfmus = ([fmu1;fmu3], [fmu2], []) in 
  let smap = [(fmu1.debugname, fmu1.fmustate); (fmu2.debugname, fmu2.fmustate); (fmu3.debugname, fmu3.fmustate)] in  
  let g = create_graph (dep) (pmap) (ivar) (ovar) in
  let start_time = {model_time = 0.0; index = 0} in 
  let end_time = {model_time = 0.0; index = 0} in 
  let topo = List.rev (result_topological_sort g) in 
  print_graph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); print_all_nodes topo;
  runSimulation (fmicounteradder) allfmus (topo) pmap smap start_time end_time start_time 2.0; ()


let testdiscretedelay =
  let _ = uprint_string (us "periodic_clock -- counter -- discrete delay");uprint_newline() in
  let fmu1 = {fmuinputs = []; fmuoutputs = [us "a"]; fmudependecies =[]; fmustate = initialize_state_periodic_clock (us "a") 2.0; debugname = (us "periodic_clock0")} in
  let fmu2 = {fmuinputs = [us "b"]; fmuoutputs = [us "c"]; fmudependecies =[(us "b", us "c")]; fmustate = initialize_state_counter (us "b") (us "c") 0.0; debugname = (us "counter")} in
  let fmu3 = {fmuinputs = [us "d"]; fmuoutputs = [us "e"]; fmudependecies =[(us "d", us "e")]; fmustate =  initialize_state_discrete_time_delay  [(us "d"); (us "e")] 2.0; debugname = (us "discretedelay")} in
  let ivar = List.append fmu1.fmuinputs (List.append fmu2.fmuinputs fmu3.fmuinputs)  in 
  let ovar = List.append fmu1.fmuoutputs (List.append fmu2.fmuoutputs fmu3.fmuinputs) in
  let pmap = [((us "b"), (us "a")); ((us "d"), (us "c"))] in 
  let dep = List.append fmu1.fmudependecies (List.append fmu2.fmudependecies fmu3.fmudependecies) in 
  let fmisimplecounter = {fmuinstances = [fmu1; fmu2; fmu3]; allinputvar = ivar; alloutputvar = ovar; globaldependencies = dep; portmapping = pmap; fminame = us "discretedelay"} in
  let allfmus = ([fmu1;fmu2;fmu3], [], []) in 
  let smap = [(fmu1.debugname, fmu1.fmustate); (fmu2.debugname, fmu2.fmustate); (fmu3.debugname, fmu3.fmustate)] in  
  let g = create_graph (dep) (pmap) (ivar) (ovar) in
  let start_time = {model_time = 0.0; index = 0} in 
  let end_time = {model_time = 21.0; index = 0} in 
  let topo = List.rev (result_topological_sort g) in 
  print_graph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); print_all_nodes topo;
  runSimulation (fmisimplecounter) allfmus (topo) pmap smap start_time end_time start_time 2.0; () 



let testmicrostepdelay =
  let _ = uprint_string (us "periodic_clock -- discrete delay");uprint_newline() in
  let fmu1 = {fmuinputs = []; fmuoutputs = [us "a"]; fmudependecies =[]; fmustate = initialize_state_periodic_clock (us "a") 2.0; debugname = (us "periodic_clock0")} in
  let fmu2 = {fmuinputs = [us "b"]; fmuoutputs = [us "c"]; fmudependecies =[(us "b", us "c")]; fmustate = initialize_state_microstep_delay [(us "b"); (us "c")]; debugname = (us "microstepdelay")} in
  let ivar = List.append fmu1.fmuinputs fmu2.fmuinputs  in 
  let ovar = List.append fmu1.fmuoutputs fmu2.fmuoutputs in
  let pmap = [((us "b"), (us "a"))] in 
  let dep = List.append fmu1.fmudependecies fmu2.fmudependecies in 
  let fmisimplecounter = {fmuinstances = [fmu1; fmu2]; allinputvar = ivar; alloutputvar = ovar; globaldependencies = dep; portmapping = pmap; fminame = us "microstepdelay"} in
  let allfmus = ([fmu1;fmu2], [], []) in 
  let smap = [(fmu1.debugname, fmu1.fmustate); (fmu2.debugname, fmu2.fmustate)] in  
  let g = create_graph (dep) (pmap) (ivar) (ovar) in
  let start_time = {model_time = 0.0; index = 0} in 
  let end_time = {model_time = 21.0; index = 0} in 
  let topo = List.rev (result_topological_sort g) in 
  print_graph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); print_all_nodes topo;
  runSimulation (fmisimplecounter) allfmus (topo) pmap smap start_time end_time start_time 2.0; () 





 



