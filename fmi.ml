open Ustring.Op
open List

exception Fmu_error of string
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
  |SAdder of float   (* sum *)
  |SDiscrete of float * float (* timer, period *)
  |SConst of float (* const *)
  |SGain of float * signal
  |SDisreteTimeDelay of float * state list (*parameter, state list*)
  |SMicrostepDelay of state list (*state list*)
  |SIntegrator of signal * signal * signal (*last output, input, initial value *)
  |SSine of float * signal * float * float (*totalTime, sample_value, (phase + (frequency * 2 * pi))/samplingRate, 1/samplingRate*)
  |SZeroCrossingDetector signal * signal * float (* previous signal , current signal, error *) 
 
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

let fmu_debug_print s =
  uprint_string ((us "debug message:") ^. (us s)); uprint_newline()

let print_time t =
  uprint_string (us "TIME : ("); uprint_float t.model_time; uprint_string (us ","); uprint_int t.index; uprint_string (us ")"); uprint_newline ()

let print_signal sg =
  match sg with 
  | Absent() -> uprint_string (us "Absent") 
  | Present(a) -> uprint_float a

let print_port p = 
 uprint_string ((fst p) ^. (us "=")); print_signal (snd p)


