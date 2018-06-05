open Ustring.Op
open List
open Ftypes

exception FMU_error of string
(* port : name
   Note : this implementation supports only real values *)
   
type port = ustring

type superdensetime = float * int 
open Ustring.Op
open List


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


