open Ustring.Op
open List
open Fmi


(* periodic discrete signal generator*) 

let get_current_timer_value s = 
  match s.addvar with 
  | SDiscrete(t, p) -> t 
  | _ -> raise Not_found

let get_period s = 
  match s.addvar with 
  | SDiscrete(t, p) -> p 
  | _ -> raise Not_found

let get_periodic_clock s y =
  let timer = get_current_timer_value s in
  let period = get_period s in
  let local_time = s.time in
  (if ((timer = period) & (local_time.index = 1)) then Present(1.0) else Absent())

  
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

(*counter*)

let get_current_count s = 
  match s.addvar with 
  | SCounter(a) -> a 
  | _ -> raise Not_found

let get_counter s y =
  let count = get_current_count s in 
  let (ipvar, ipval) =  List.hd s.portvalue in
  let _ = print_time s.time in
  let count = get_current_count s in 
  let opval = if (ipval <> Absent()) then (count + 1) else Absent() 
  

let do_step_counter s h =
  let _ = assert(h >= 0.0) in
  let local_time = s.time in 
  let updatedtime = if (h > 0.0) then {model_time = local_time.model_time +. h; index = 0} else {model_time = local_time.model_time; index = local_time.index + 1} in   
  let count = get_current_count s in 
  let (ipvar, ipval) = List.hd s.portvalue in
  







  (*match timer with
  |period when s.index_counter > 0 -> Present(1.0)
  |period when s.index_counter = 0 -> Absent()
  |_ -> Absent()*)

