open Ustring.Op
open List


let example1 t y =
  (t *. y) +. (t *. t *. t)

let rec euler_method (func : float -> float -> float) w_now t_now t_end h =
  let w_next = w_now +. (h *. (func t_now w_now)) in
  let t_next = t_now +. h in
  let _ = uprint_float t_next; uprint_char (uc '|'); uprint_float w_next;  uprint_char (uc '|'); uprint_newline() in 
  if(t_next < t_end) then
    euler_method (func) w_next t_next t_end h 
  else 
   ()
  

let euler_method_init =
  let t_init = 0.0 in
  let t_end = 1.0 in 
  let y_init = 1.0 in
  let h = 0.2 in 
   let _ = uprint_float t_init; uprint_float y_init; uprint_float y_init; uprint_newline() in 
  euler_method example1 y_init t_init t_end h 
  



