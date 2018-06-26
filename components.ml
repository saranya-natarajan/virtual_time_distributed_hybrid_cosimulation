open Ustring.Op
open List
open Fmi


(* periodic discrete signal generator
   List.nth s.portvalue 0 : output
*)


let get_current_timer_value s = 
  match s.addvar with 
  | SDiscrete(t, p) -> t 
  | _ -> raise Not_found

let get_period s = 
  match s.addvar with 
  | SDiscrete(t, p) -> p 
  | _ -> raise Not_found

let print_periodic_clock_output s value = 
  let (prt, sgn) = List.hd s.portvalue in 
  uprint_string (prt ^. (us " = ")) ; print_signal value ;  uprint_newline() 

let get_periodic_clock s y =
  let timer = get_current_timer_value s in
  let period = get_period s in
  let local_time = s.time in
  let res = (if ((timer = period) && (local_time.index = 1)) then Present(1.0) else Absent()) in 
  (print_periodic_clock_output s res); res  
 
let do_step_periodic_clock s h =
  let _ = assert(h >= 0.0) in
  let local_time = s.time in 
  let timer = get_current_timer_value s in
  let period = get_period s in
  let (supdated, haccepted) = (if (h <> 0.0) then 
                                (match s.addvar with 
                                |SDiscrete(t, p) when (t <= h) -> let updatedtime = {model_time = local_time.model_time +. t; index = 0} in 
                                                                  ({s with time = updatedtime; addvar = SDiscrete(0.0, p)}, t)
                                |SDiscrete(t, p) when (t > h) -> let updatedtime ={model_time = local_time.model_time +. h; index = 0} in 
						                 ({s with time = updatedtime; addvar = SDiscrete((t -. h), p)}, h))
                             else
                               (match s.addvar with 
                                 |SDiscrete(t, p) when t = 0.0 -> let updatedtime = {model_time = local_time.model_time; index = local_time.index + 1} in 
                                      ({s with time = updatedtime; addvar = SDiscrete(p, p)}, 0.0)
                                 |SDiscrete(t, p) when t <> 0.0 -> let updatedtime = {model_time = local_time.model_time; index = local_time.index + 1} in 
                                      ({s with time = updatedtime; addvar = SDiscrete(t, p)}, 0.0)))
  in
   (supdated, haccepted)

let get_max_step_size_periodic_clock s  =
  let timer = get_current_timer_value s in
  let period = get_period s in
  let local_time = s.time in
  let opval = (if ((timer = period) && (local_time.index = 1)) then Present(1.0) else Absent()) in 
  (match s.addvar with
  |SDiscrete(t, p) -> (match opval with 
                      |Absent() -> t
                      |Present(_) -> 0.0)
  |_ -> raise Not_found)



let initialize_state_periodic_clock portvar period = 
   {portvalue = [(portvar, Absent())]; time = {model_time = 0.0; index =0}; addvar = SDiscrete(0.0, period)}

(*counter
  List.nth s.portvalue 0 : input to counter
  List.nth s.portvalue 1 : output
*)


let get_current_count s = 
  match s.addvar with 
  | SCounter(a) -> a 
  | _ -> raise Not_found

let get_max_step_size_counter s  =
  let (ipvar, ipval) = List.nth s.portvalue 1 in
  (match ipval with 
  |Absent() -> infinity 
  |Present(_) -> 0.0)


let get_counter s y =
  let count = get_current_count s in 
  let (ipvar, ipval) =  List.hd s.portvalue in
  let (opvar, _) = List.nth s.portvalue 1 in
  let count = get_current_count s in 
  let opval = if (ipval <> Absent()) then Present(count +. 1.0) else Absent() in
  uprint_string (opvar ^. (us "=")); print_signal opval;  uprint_newline(); opval 
  

let do_step_counter s h =
  let (ipvar, ipval) =  List.hd s.portvalue in
  let local_time = s.time in 
  let updatedtime = if (h <> 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in
  let count = get_current_count s in
  (match ipval with 
   |Absent() -> ({s with time = updatedtime}, h)
   |Present(_) -> ({s with time = updatedtime; addvar = SCounter(count +. 1.0)}, h))
 

   
let initialize_state_counter inputvar outputvar initcount= 
   {portvalue = [(inputvar, Absent()); (outputvar, Absent())]; time = {model_time = 0.0; index =0}; addvar = SCounter(initcount)} 


(*adder 
  List.nth s.portvalue 0 : input to add
  List.nth s.portvalue 1 : input to subtract
  List.nth s.portvalue 2 : output 
*)

let get_max_step_size_adder s  =
  let (addvar, addval) = List.nth s.portvalue 0 in
  let (subvar, subval) = List.nth s.portvalue 1 in  
  (match addval with 
  |Absent() -> (match subval with 
	        |Absent() -> infinity
                |Present(a) -> 0.0)
  |Present(_) -> 0.0)

let get_current_sum s = 
  match s.addvar with 
  | SAdder(a) -> a 
  | _ -> raise Not_found

let get_adder s y =
  let sum = get_current_sum s in 
  let (addvar, addval) = List.nth s.portvalue 0 in
  let (subvar, subval) = List.nth s.portvalue 1 in
  let updatedsum = (match addval with 
                   |Present(a) -> (match subval with 
                                        |Present(b) -> Present(sum +. a -. b)
                                        |Absent() -> Present(sum +. a))
                   |Absent() -> (match subval with 
			       |Present(b) -> Present(sum -. b) 
                               |Absent() -> Present(sum))) in 
   uprint_string (fst (List.nth s.portvalue 2) ^. (us "=")); print_signal updatedsum;  uprint_newline(); updatedsum

let do_step_adder s h = 
  let local_time = s.time in 
  let updatedtime = if (h <> 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in
  let sum = get_current_sum s in
  let (addvar, addval) = List.nth s.portvalue 0 in
  let (subvar, subval) = List.nth s.portvalue 1 in 
  let updatedstate = (match addval with 
                   |Present(a) -> (match subval with 
                                   |Present(b) -> SAdder(sum +. a -. b)
                                   |Absent() -> SAdder(sum +. a))
                   |Absent()   -> (match subval with 
			           |Present(b) -> SAdder(sum -. b) 
                                   |Absent() -> SAdder(sum))) in 
    ({s with time = updatedtime; addvar = updatedstate}, h)

let initialize_state_adder allvar initcount= 
   {portvalue = [((List.nth allvar 0), Absent()); ((List.nth allvar 1), Absent()); ((List.nth allvar 2), Absent())]; time = {model_time = 0.0; index =0}; addvar = SAdder(initcount)} 

(*constant signal generator
  List.nth s.portvalue 0 : output 
*)

let get_max_step_size_const s  =
  infinity 

let get_const s y =
   let opval = match s.addvar with
               |SConst(c) -> Present(c)
               |_ -> raise Not_found in
   uprint_string (fst (List.nth s.portvalue 0) ^. (us "=")); print_signal opval ;  uprint_newline(); opval

let do_step_const s h = 
  let local_time = s.time in 
  let updatedtime = if (h <> 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in
  ({s with time = updatedtime}, h)

let initialize_state_const allvar initcount= 
   {portvalue = [((List.nth allvar 0), Present(initcount))]; time = {model_time = 0.0; index =0}; addvar = SConst(initcount)} 

(* Gain
  List.nth s.portvalue 0 : input 
  List.nth s.portvalue 1 : output 
*)

let gain_value s =
  match s.addvar with 
  |SGain(factor, pout) -> factor
  |_ -> raise Not_found

let gain_previous_output s =
  match s.addvar with 
  |SGain(factor, pout) -> pout
  |_ -> raise Not_found


let get_gain s y =
   let(ipvar, ipval) = List.nth s.portvalue 0 in
   let factor = gain_value s in  
   let opval = (match ipval with
               |Absent() -> Absent()
               |Present(a) -> Present(a *. factor)) in
   uprint_string (fst (List.nth s.portvalue 1) ^. (us "=")); print_signal opval ;  uprint_newline(); opval

let do_step_gain s h = 
  let local_time = s.time in 
  let(ipvar, ipval) = List.nth s.portvalue 0 in
  let pout = gain_previous_output s in
  let factor = gain_value  s in
  (match pout with 
   |Absent() -> let updatedtime = if (h <> 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in
                 (match ipval with
                 |Absent() -> ({s with time = updatedtime}, h)
                 |Present(a) -> ({s with time = updatedtime; addvar = SGain(factor, Present(factor *. a))}, h))
   |Present(a) -> (match ipval with 
	          |Absent() -> ({s with time = {model_time = local_time.model_time; index = local_time.index + 1}; addvar = SGain(factor, Absent())}, 0.0) 
                  |Present(a) -> let updatedtime = if (h <> 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in
                                  ({s with time = updatedtime; addvar = SGain(factor, Present(factor *. a))}, h)))

 
let initialize_state_gain allvar initcount= 
   {portvalue = [((List.nth allvar 0), Absent()); ((List.nth allvar 1), Absent())]; time = {model_time = 0.0; index = 0}; addvar = SGain(initcount, Absent())} 



(* Discrete time delay
  List.nth s.portvalue 0 : input 
  List.nth s.portvalue 1 : output 
*)


let get_max_step_size_discrete_time_delay s  =
  infinity

let get_discrete_time_delay s y =
  let local_time = s.time in
  let (d, statelist) = (match s.addvar with 
                         |SDisreteTimeDelay(d, slist) -> (d, slist)
                         |_ -> raise Not_found) in
  let op_state_option = try Some(List.find (fun a -> let st = a.time in (st.model_time = local_time.model_time -. d) && (st.index = local_time.index)) statelist)
                         with Not_found -> None in 
  let opval = if (local_time.model_time >= d) then 
                  (match op_state_option with
                  |Some(s)-> let (ipvar, ipval) = List.hd s.portvalue in 
                                  ipval
                  |None -> Absent()) 
               else 
                  (Absent()) in
   uprint_string (fst (List.nth s.portvalue 1) ^. (us "=")); print_signal opval ;  uprint_newline(); opval

let do_step_discrete_time_delay s h = 
  let local_time = s.time in 
  let(ipvar, ipval) = List.nth s.portvalue 0 in
  let updatedtime = if (h <> 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in
  let updatedstate = (match s.addvar with 
                           |SDisreteTimeDelay(d, slist) -> {s with time=updatedtime; addvar=SDisreteTimeDelay(d, s :: slist)}
                           |_ -> uprint_string (us "expected SDisreteTimeDelay"); raise Not_found) in
  (updatedstate, h)
 
let initialize_state_discrete_time_delay allvar para = 
   {portvalue = [((List.nth allvar 0), Absent()); ((List.nth allvar 1), Absent())]; time = {model_time = 0.0; index = 0}; addvar = SDisreteTimeDelay(para, [])} 


(* microstep delay
  List.nth s.portvalue 0 : input 
  List.nth s.portvalue 1 : output 
*)


let get_max_step_size_microstep_delay s  =
  infinity

let get_microstep_delay s y =
  let local_time = s.time in
  let opval = if (local_time.index <> 0) then 
                (let statelist = (match s.addvar with 
                                  |SMicrostepDelay(slist) -> slist
                                  |_ -> raise Not_found) in
                let op_state_option = try Some(List.find (fun a -> let st = a.time in (st.model_time = local_time.model_time) && (st.index = local_time.index - 1)) statelist)
                                    with Not_found -> None in 
                (match op_state_option with
                |Some(s)-> let (ipvar, ipval) = List.hd s.portvalue in ipval
                |None -> Absent()))
              else 
                (Absent()) in
   uprint_string (fst (List.nth s.portvalue 1) ^. (us "=")); print_signal opval ;  uprint_newline(); opval

let do_step_microstep_delay s h = 
  let local_time = s.time in 
  let(ipvar, ipval) = List.nth s.portvalue 0 in
  let updatedtime = if (h <> 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in
  let updatedstate = (match s.addvar with 
                           |SMicrostepDelay(slist) -> {s with time=updatedtime; addvar=SMicrostepDelay(s :: slist)}
                           |_ -> uprint_string (us "expected SMicrostepDelay"); raise Not_found) in
  (updatedstate, h)
 
let initialize_state_microstep_delay allvar = 
   {portvalue = [((List.nth allvar 0), Absent()); ((List.nth allvar 1), Absent())]; time = {model_time = 0.0; index = 0}; addvar = SMicrostepDelay([])} 


(* integrator 
  List.nth s.portvalue 0 : input 
  List.nth s.portvalue 1 : output 
*)
let euler_method_test op_now h t =
  let ip_now = (t *. op_now) +. (t *. t *. t) in
  let op_next = op_now +. (h *. ip_now) in
  op_next

let euler_method ip_now op_now h = 
  let op_next = op_now +. (h *. ip_now) in
  op_next

  

let get_max_step_size_integrator s  =
  infinity

let get_integrator s y =
  let opval = (match s.addvar with 
               |SIntegrator(op, _, _) -> op
               | _ -> raise (Fmu_error "not integrator") ) in
  uprint_string (fst (List.nth s.portvalue 1) ^. (us "=")); print_signal opval ;  uprint_newline(); opval

let do_step_integrator s h = 
  let local_time = s.time in 
  let(ipvar, ipval) = List.nth s.portvalue 0 in
  let updatedtime = if (h <> 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in
  let (opst, ipst, init) =  (match s.addvar with 
               |SIntegrator(op, ip, init) -> (op, ip, init)
               | _ -> raise (Fmu_error "not integrator")) in
  let opval = (match opst with 
               |Present(a) -> (match ipval with 
                               |Present(b) -> Present(euler_method_test a h local_time.model_time)
                               |Absent() -> Absent())
               | Absent() ->  raise (Fmu_error "some serious error in integrator"))  in
  let updatedstate = {s with time = updatedtime; addvar = SIntegrator(opval, ipval, init)} in 
  (updatedstate, h)  
 
let initialize_integator allvar initval = 
   {portvalue = [((List.nth allvar 0), Present(List.nth initval 0)); ((List.nth allvar 1), Present(List.nth initval 1))]; time = {model_time = 0.0; index = 0}; addvar = SIntegrator(Present(List.nth initval 0), Present(List.nth initval 1), Present(List.nth initval 2))}


(*sine wave 
  List.nth s.portvalue 0 : output
*)


let get_max_step_size_sinewave s  =
  infinity


let get_sinewave s y =
  let (omega, opval, step) = (match s.addvar with
        			         |SSine(oval, pval, step) -> (oval, pval, step)
           			         | _ -> raise (Fmu_error "not sinewave"))  in
   uprint_string (fst (List.nth s.portvalue 0) ^. (us "=")); print_signal opval ;  uprint_newline(); opval

let do_step_sinewave s h =
  let local_time = s.time in 
  let(ipvar, ipval) = List.nth s.portvalue 0 in
  let updatedtime = if (h <> 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in



   




  

