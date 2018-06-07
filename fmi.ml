open Ustring.Op
open List

exception FMU_error of string
(* port : name
   Note : this implementation supports only real values *)
   
type port = ustring

type superdensetime = 
  {
    model_time : float;
    index : int;
  }
 
type signal = 
  |Present of float 
  |Absent  of unit

and svar = 
  |SCounter of float (* count *)
  |SDiscrete of float * float (* timer, period *) 
  

and step =
  | Default of unit 
  | Variable of unit 
  | None

and debug = ustring 

and state = 
  {
    portvalue : (port * signal) list;
    time : superdensetime; 
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
    fminame : ustring;
  }

and graph = 
  {
    nodes : port list;
    edges : (port * port) list;
  }

let print_time t =
  uprint_string (us "TIME : ("); uprint_float t.model_time; uprint_string (us ","); uprint_int t.index; uprint_string (us ")"); uprint_newline ()

let print_signal sg =
  match sg with 
  | Absent() -> uprint_string (us "Absent") 
  | Present(a) -> uprint_float a


