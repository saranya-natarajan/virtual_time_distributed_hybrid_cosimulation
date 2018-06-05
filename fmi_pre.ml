open Ustring.Op
open List


exception FMU_error of string
(* port : name
   Note : this implementation supports only real values *)
   
type port = ustring

type superdensetime = float * int 

type signal = 
  |Present of float 
  |Absent  of unit

and svar = 
  |SCounter of float (* count *)
  |SDiscrete of float * float (* timer period *) 
  

and step =
  | Default of unit 
  | Variable of unit 
  | None

and debug = ustring 

and state = 
  {
    portvalue : (port * signal) list;
    time : (float * int); 
    index_counter : int;
    addvar:  svar; 
  }
   
and fmu = 
  {
    fmuinputs : port list;
    fmuoutputs : port list;
    fmudependecies : (port * port) list; 
    fmustate : state;
    (*funset : state -> port -> float -> state;
    funget : state -> port -> float;
    fundostep : state -> float -> state * float;  
    fun_get_max_step_size : state -> float; *)
    debugname : debug ;(* Only applicable for this implementation *)  
  }

and fmi =
  {
    fmuinstances : fmu list;
    allinputvar : port list;
    alloutputvar : port list;
    globaldependencies : ( port *  port) list;
    portmapping : (port * port) list;
    debugname : ustring;
  }

and graph = 
  {
    nodes : port list;
    edges : (port * port) list;
  }

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

let get_current_count s = 
  match s.addvar with 
  | SCounter(a) -> a 
  | _ -> raise Not_found

let get_current_timer_value s = 
  match s.addvar with 
  | SDiscrete(t, p) -> t 
  | _ -> raise Not_found


let get_period s = 
  match s.addvar with 
  | SDiscrete(t, p) -> p 
  | _ -> raise Not_found


let do_step_counter s h =
  let _ = assert(h >= 0.0) in
  let (modeltime, index) = s.time in 
  let updatedtime = if (h > 0.0) then (modeltime +. h, 0) else (modeltime, index + 1) in   
  let count = get_current_count s in 
  let (incvar, incval) = List.hd s.portvalue in
  let supdated  = (if h = 0.0 then  
       		    (match incval with 
                    | Present(a) when (s.index_counter = 0)  -> {s with addvar = SCounter(count +. 1.0); time = updatedtime; index_counter = s.index_counter + 1} 
                    | Present(a) when (s.index_counter <> 0) -> {s with time = updatedtime;  index_counter = s.index_counter + 1}
                    | Absent() -> {s with time = updatedtime; index_counter = 0}) 
                  else
                    (match incval with 
                    | Absent() -> {s with time = updatedtime; index_counter = 0}
                    | Present(a) ->  {s with addvar = SCounter(count +. 1.0); time = updatedtime; index_counter = 1}))
                in
  (supdated, h)
  
let do_step_periodic_clock s h =
  let _ = assert(h >= 0.0) in
  let (modeltime, index) = s.time in 
  let timer = get_current_timer_value s in
  let period = get_period s in
  let (supdated, haccepted) = (if (h <> 0.0) then 
                                (match s.addvar with 
                                |SDiscrete(t, p) when (t <= h) -> let updatedtime = ((modeltime +. t), 0) in 
                                                                  ({s with time = updatedtime; index_counter = 0; addvar = SDiscrete(0.0, p)}, t)
                                |SDiscrete(t, p) when (t > h) -> let updatedtime = ((modeltime +. h), 0) in 
						                 ({s with time = updatedtime; index_counter = 0; addvar = SDiscrete((t -. h), p)}, h))
                             else
                               (match s.addvar with 
                                 |SDiscrete(t, p) when t = 0.0 -> let updatedtime = ((modeltime), index+1) in 
                                      ({s with time = updatedtime; index_counter = s.index_counter + 1; addvar = SDiscrete(p, p)}, 0.0)
                                 |SDiscrete(t, p) when t = p -> let updatedtime = ((modeltime), index+1) in 
                                      ({s with time = updatedtime; index_counter = s.index_counter + 1; addvar = SDiscrete(t, p)}, 0.0)
                                 |SDiscrete(t, p) -> let updatedtime = ((modeltime), index+1) in 
                                      ({s with time = updatedtime; index_counter = 0; addvar = SDiscrete(t, p)}, 0.0)))

in
   (supdated, haccepted)

let get_counter s y =
  let count = get_current_count s in 
  let (incvar, incval) =  List.hd s.portvalue in
   let _ = uprint_string (us "local time") ; uprint_string (us "("); uprint_float (fst s.time); uprint_string (us ",");  uprint_int (snd s.time); uprint_string (us ")") in
  (*let _ = uprint_string (us "counter"); uprint_int s.index_counter in *) 
  (*(if (s.index_counter = 0) then Absent() else Present(count)) *)
  match incval with
  |Absent() -> Absent()
  |Present(a) when s.index_counter = 0 -> Present(count +. 1.0)
  |Present(a) when s.index_counter <> 0 -> Present(count)


let get_periodic_clock s y =
  let timer = get_current_timer_value s in
  let period = get_period s in
  (if (s.index_counter = 0) then Absent() else Present(1.0))
  (*match timer with
  |period when s.index_counter > 0 -> Present(1.0)
  |period when s.index_counter = 0 -> Absent()
  |_ -> Absent()*)
   
 
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


let print_time t =
  let (m, i) = t in
  uprint_string (us "TIME : ("); uprint_float m; uprint_string (us ","); uprint_int i; uprint_string (us ")"); uprint_newline ()

let print_signal sg =
  match sg with 
  | Absent() -> uprint_string (us "Absent") 
  | Present(a) -> uprint_float a

let print_state fname s = 
    uprint_string ((us "FMU ") ^. (fname)); uprint_newline (); 
    uprint_string (us "port value") ; uprint_newline (); 
    List.iter (fun a -> uprint_string (fst a); uprint_string (us " ="); print_signal (snd a)) s.portvalue;
    uprint_string (us "model time") ; uprint_string (us "("); uprint_float (fst s.time); uprint_string (us ",");  uprint_int (snd s.time); uprint_string (us ")"); ()  


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
  let _ = output_of_model fmiset.debugname smap in 
  (*check : let _ = List.iter (fun a -> print_state (fst a) (snd a)) smap in  *)
  let h = step_two allfmus smap hmax in 
  let r = step_three allfmus smap in 
  let (m, h) = rollback_on_accepted_step allfmus smap r h in (*step 4 to step 6*)
  let (mprime, hprime) = step_eight allfmus m h in (* skipping step 7 since no legacy supported in this implementation *)
  if h <> hprime then raise Not_found else (mprime, hprime) 

   
let initialize_state_periodic_clock portvar period = 
   {portvalue = [(portvar, Absent())]; time = (0.0, 0); index_counter = 0; addvar = SDiscrete(0.0, period)}
   
let initialize_state_counter inputvar outputvar initcount= 
   {portvalue = [(inputvar, Absent()); (outputvar, Absent())]; time = (0.0, 0); index_counter = 0; addvar = SCounter(initcount)}


let rec runSimulation fmiinstance allfmus orderedlist pmap m startime endtime timenow h =  
  let (mprime, hprime) = masterStepSuperdenseTime fmiinstance allfmus orderedlist pmap h m in 
   let _ = uprint_string (us "h = "); uprint_float h; uprint_newline() in
  let _ = assert(hprime>=0.0) in
  let _ = uprint_string (us "accepted h = "); uprint_float hprime; uprint_newline() in 
  let _ =  uprint_newline () in
  let (modeltime, index) = timenow in 
  let timenow = if (hprime = 0.0) then (modeltime, index + 1) else (modeltime +. hprime, 0) in     
  let _ = uprint_string (us "time = ( "); uprint_float (fst timenow);  uprint_string (us ","); uprint_int (snd timenow); uprint_string (us ")");  uprint_newline() in
  if (fst timenow < fst endtime) then runSimulation fmiinstance allfmus orderedlist pmap mprime startime endtime timenow 2.0 else () 
   
let testSimplestCounter =
  let fmu1 = {fmuinputs = []; fmuoutputs = [us "a"]; fmudependecies =[]; fmustate = initialize_state_periodic_clock (us "a") 3.0; debugname = (us "periodic_clock0")} in
  let fmu2 = {fmuinputs = [us "b"]; fmuoutputs = [us "c"]; fmudependecies =[(us "b", us "c")]; fmustate = initialize_state_counter (us "b") (us "c") 0.0; debugname = (us "counter")} in
  let ivar = List.append fmu1.fmuinputs fmu2.fmuinputs in 
  let ovar = List.append fmu1.fmuoutputs fmu2.fmuoutputs in
  let pmap = [((us "b"), (us "a"))] in 
  let dep = List.append fmu1.fmudependecies fmu2.fmudependecies in 
  let fmisimplecounter = {fmuinstances = [fmu1; fmu2]; allinputvar = ivar; alloutputvar = ovar; globaldependencies = dep; portmapping = pmap; debugname = us "simplestcounter"} in
  let (cp, cr, cl) = ([fmu1], [fmu2], []) in 
  let smap = [(fmu1.debugname, fmu1.fmustate); (fmu2.debugname, fmu2.fmustate)] in  
  let g = create_graph (dep) (pmap) (ivar) (ovar) in
  let topo = result_topological_sort g in 
  print_graph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); print_all_nodes topo;
  runSimulation fmisimplecounter (cp, cr, cl) (topo) pmap smap (0.0, 0) (20.0, 0) (0.0, 0) 2.0; ()


(*

let counter_solver s h res =
  let ipone = (match s.input_0 with 
                  |Present(value) -> value
                  |Absent() -> 0.0
                  |Null() -> raise (FMU_error "input 0 of counter is NULL")) in  
  let iptwo = (match s.input_1 with 
                  |Present(value) -> value
                  |Absent() -> 0.0
                  |Null() -> raise (FMU_error "input 2 of counter is NULL")) in
  let (m, i) = s.time in 
  let updatedtime = if (h = 0.0) then (m, i+1) else (m +.h, 0) in
  if (ipone <> 0.0 && iptwo <> 0.0) then (
  let count = res +. 1.0 in 
  let smap = if count <> 0.0 then {s with output_0 = Present(count); interface = SCounter(count); time = updatedtime} else ({s with output_0 = Absent(); interface = SCounter(0.0); time = updatedtime} ) in 
   (smap, h))
  else 
  (let smap = ({s with output_0 = Absent(); interface = SCounter(res); time = updatedtime} ) in 
   (smap, h))

 
   

let updateOutputDiscrete op (model, index) =
  if (index = 1) then Present(1.0) else Absent()

let discrete_solver_microstep s h =
  let (model, index) = s.time in
  let updatedtime = (model, index + 1) in 
  if ((mod_float model s.maxstep) = 0.0 )then ( 
  (match (snd updatedtime) with 
   | 0 -> ({s with output_0 = Absent(); time = updatedtime}, h)
   | 1 ->  ({s with output_0 = Present(1.0); time = updatedtime}, h)
   | 2 -> ({s with output_0 = Absent(); time = updatedtime}, h)
   | _ ->  ({s with output_0 = Absent(); time = updatedtime}, h)
  ))
  else ({s with time = updatedtime}, h)

let discrete_solver_mul s h =
  let (model, index) = s.time in
  let updatedtime = (model +. h, 1) in 
  if ((mod_float (fst updatedtime) s.maxstep) = 0.0 ) then ({s with output_0 = Present(1.0); time = updatedtime}, h) else  ({s with output_0 = Absent(); time = updatedtime}, h)

let rec discrete_solver s h =
  if (h = 0.0) then discrete_solver_microstep s h else discrete_solver_modelstep s h 
and discrete_solver_modelstep s h = 
  let (model, index) = s.time in
  let updatedtime = (model +. h, 1) in
  if (abs_float ((fst updatedtime) -. model) > s.maxstep) then (discrete_solver s s.maxstep) else 
      (if (abs_float ((fst updatedtime) -. model) < s.maxstep) then  discrete_solver_mul s h else 
                  ({s with output_0 = Present(1.0); time = updatedtime}, h))
                
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
  |SDiscrete() ->  discrete_solver s h  

 

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
                             let smap = List.map (fun a -> if (fst a = findFMUOfPort u) then (fst a, m_u_new) else a) m in 
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
  if (hmin < h) then (let smaprestored = step_four allfmus r in rollbackOnAcceptedStep allfmus smaprestored r hmin) else (smapprime, hprime)

let rec step_seven allfmus smap h = 
  let cp = allFMUWithPredictableStepSize allfmus in 
  let (smapprime, hprime) = doStepOnFMU cp smap h in 
  (smapprime, hprime)


let masterStepSuperdenseTime allfmus orderedlist pmap hmax m =
  (*step one and check*)
  (*List.iter (fun a -> printState (fst a) (snd a)) m ; *)
  let smap = step_one orderedlist m pmap in 
  List.iter (fun a -> printState (fst a) (snd a) ) smap ; 
  (*step two and check*)
  let h = step_two allfmus smap hmax in
  (*step two and check*)
  let r = step_three allfmus smap in
  (*step 4 to step 6 on repeat*)
  let (m, h) =  rollbackOnAcceptedStep allfmus smap r h in 
  let (mprime, hprime) = step_seven allfmus m h in
  uprint_float h; uprint_float hprime; 
  if h <> hprime then raise Not_found else (mprime, hprime)

let final_run orderedlist pmap mprime timenow = 
  let timenow = (fst timenow, (snd timenow) + 1) in  
  let sm = step_one orderedlist mprime pmap in 
  (List.iter (fun a -> printState (fst a) (snd a) ) sm );  printTime timenow ; uprint_newline ()


let rec runSimulation allfmus orderedlist pmap m startime endtime timenow =
  let _ = uprint_string (us "SIMULATION"); uprint_newline() in
  let (mprime, hprime) = masterStepSuperdenseTime allfmus orderedlist pmap 3.0 m in
  let timenow = if (hprime = 0.0) then (fst timenow, (snd timenow) + 1) else ((fst timenow) +. hprime, 0) in
  let _ = printTime timenow ; uprint_newline () in 
  let (mprime, hprime) = masterStepSuperdenseTime allfmus orderedlist pmap 0.0 mprime in 
  let timenow = if (hprime = 0.0) then (fst timenow, (snd timenow) + 1) else ((fst timenow) +. hprime, 0) in
  let _ = printTime timenow ; uprint_newline () in 
  let _ = final_run orderedlist pmap mprime timenow in 
  if (fst timenow < fst endtime) then runSimulation allfmus orderedlist pmap mprime startime endtime timenow else () 

let statediscreteclock step = {input_0 = Null(); input_1 = Null(); input_2 = Null(); input_3 = Null(); input_4 = Null(); output_0 = Absent(); output_1 = Null(); output_2 = Null(); output_3 = Null(); output_4 = Null(); maxstep = step; time = (0.0, 0); interface = SDiscrete()}

let statecounter init = {input_0 = Absent(); input_1 = Absent(); input_2 = Null(); input_3 = Null(); input_4 = Null(); output_0 = Absent(); output_1 = Null(); output_2 = Null(); output_3 = Null(); output_4 = Null(); maxstep = infinity; time = (0.0, 0); interface = SCounter(init)}

 
(*Test function*)
let testSimpleCounter =
  let fmu1 = {name = us  "discreteclock1"; inputs = []; outputs = [Output_0(us "discreteclock1")]; dependecies = []; state = statediscreteclock 2.0 } in 
  let fmu2 = {name =us  "discreteclock2"; inputs = []; outputs = [Output_0(us "discreteclock2")]; dependecies = [];  state =  statediscreteclock 4.0 } in 
  let fmu3 = {name = us "counter"; inputs = [Input_0(us "counter") ; Input_1(us "counter")]; outputs = [Output_0(us "counter")]; dependecies = [(Input_0(us "c"), Output_1(us "e")); (Input_1(us "d"), Output_0(us "e"))]; state = statecounter 0.0} in
  let iVar = List.append fmu1.inputs (List.append fmu2.inputs fmu3.inputs) in
  let oVar = List.append fmu1.outputs (List.append fmu2.outputs fmu3.outputs) in
  let pMap = [(Input_0(us "counter"), Output_0(us "discreteclock1")); (Input_1(us "counter"), Output_0(us "discreteclock2"))] in 
  let dep =  [(Input_0(us "counter"), Output_0(us "counter")); (Input_1(us "counter"), Output_0(us "counter"))] in
  let fmisimplecounter = {fmuInstances = [fmu1; fmu2; fmu3]; allinputVar = iVar; alloutputVar = oVar; globalDependencies = dep; portMaping = pMap} in
  let (cp, cr, cl) = ([fmu1; fmu2],  [fmu3], []) in
  let sMap = [(fmu1.name, fmu1.state); (fmu2.name, fmu2.state); (fmu3.name, fmu3.state)] in
  let g = createGraph (dep) (pMap) (iVar) (oVar) in
  let topo = resTopologicalSort g in 
  printGraph g; uprint_string (us "TOPOLOGICAL ORDER: ");uprint_newline (); printAllNodes topo;
  runSimulation (cp, cr, cl) topo pMap sMap (0.0, 0) (20.0, 0) (0.0, 0); () 

 

 *)
