(***************************************************************************
 *   Copyright (C) 2012  Antoine Bodin <antoinexp@gmail.com>               *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or         *
 *   modify it under the terms of the GNU General Public License           *
 *   as published by the Free Software Foundation; either version 2        * 
 *   of the License, or (at your option) any later version.                *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA          *
 ***************************************************************************)

(** pattern.ml *)

open Perceptron
open Graphics

let width,height = 9,9
let scale = 30


let draw_grid t =
  draw_segments (Array.init height (fun i -> (0,scale*i,scale*width,scale*i)));
  draw_segments (Array.init width (fun i -> (scale*i,0,scale*i,scale*height)));
  Array.iteri (fun i ->
    Array.iteri (fun j t -> 
      if t=1. then fill_rect (scale*j) (scale*i) scale scale
    )) t


let get_image () =
  let t = Array.make_matrix height width 0. in
  let loop = ref true in
    clear_graph ();
    draw_grid t;
    while (!loop) do
      if button_down () then
	  let e = wait_next_event [Mouse_motion;Poll] in
	    (t.(e.mouse_y/scale).(e.mouse_x/scale)<-1.; draw_grid t)
      else (
	  let e = wait_next_event [Key_pressed;Button_down] in
	    if e.key = ' ' then loop:=false
	    else if e.button then (t.(e.mouse_y/scale).(e.mouse_x/scale)<-1.; draw_grid t))
    done; t


let print_res r = 
  let m = ref 0 in
    print_endline "result: ";
    Array.iteri (fun i p -> 
      if p>r.(!m) then m:=i;
      Printf.printf "%d : %.2f%%\n" (i+1) (p*.100.)
    ) r; Printf.printf "-> pattern n°%d\n" (!m+1);
    flush stdout


let main n =
  let res k = Array.init n (function i when i=k -> 1. | _ -> 0.) in
  let nt = Network.make (width*height) [|9;4;n|] in
    open_graph "";
    resize_window (scale*width) (scale*height);
  let base = Array.init n (fun i -> 
    Printf.printf "draw pattern n°%d\n" (i+1); flush stdout;
    let t = get_image () in
      Array.concat (Array.to_list t), res i)
  in print_endline "learning"; 
    Network.training nt base 500; 
    print_endline "checking";
    Array.iter (fun (s,_) -> print_res (Network.eval nt s)) base;
    while true do
      let s = Array.concat (Array.to_list (get_image ())) in
	print_res (Network.eval nt s);
    done; close_graph ()


let _ = 
  Random.self_init (); 
  print_endline "array length:";
  main (read_int ())
  
