(***************************************************************************
 *   Copyright (C) 2011  Antoine Bodin <antoinexp@gmail.com>               *
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

(** perceptron.ml *)

open Math
open Vector
open Matrix
(*open Perceptron*)

module Layer =
  struct
    type t = {
	w:Matrix.t;  (** w.(id neuron).(id weigh) *)
	b:Vector.t;  (** b.(id neuron) bias coeff *)
	s:Vector.t;  (** s.(id neuron) last computed sensibility *)
	n:Vector.t;  (** n.(id neuron) last computed output *)
	a:Vector.t;  (** a.(id neuron) = f n.(id neuron)*)
	f:float->float; (** activation function (just below) *)
	f':float->float; (** derivative of the activation function *)
	g:float;    (** learning rate *)
    }
	
    (** activation function *)
    let sigmoid a = (fun x -> 1. /. (1.+.exp (-.x/.a))), 
      (fun x -> let b = exp (-.x/.a) in b /. (a*.((1.+.b)**2.)))


    (** [make r s (f,f')] creates a layer of [s] neurons with [r] entries *)
    let make r s ?(g = 0.1) (f,f') =
      {w=Matrix.init s r (fun _ _ -> (Random.float 2.)-.1.);
       b=Vector.init s (fun _ -> (Random.float 2.)-.1.);
       n=Vector.make s 0.;
       a=Vector.make s 0.;
       s=Vector.make s 0.;
       f=f; f'=f'; 
       g=g;
      }

    (** [eval la p] is the output of the layer [la] with [p] *)
    let eval la p =
      Vector.set la.n ((la.w *|/ p) -/ la.b);
      for i=0 to dim la.a - 1 do
	la.a.(i)<-la.f la.n.(i)
      done; la.a

    (** [update la p] updates then layer [la] with the input [p] *)
    let update la p =
      let dw = (Matrix.of_vector la.s) *| (Matrix.to_line p) in
      Matrix.set la.w (la.w +| (la.g |* dw));
      Vector.set la.b (la.b -/ (la.g /* la.s))

    (** used for the last layer in a network (see below) *)
    let last_retropropagate la d =
      let e = d -/ la.a in
	for i=0 to dim e-1 do
	  la.s.(i)<- (la.f' la.n.(i))*.e.(i)
	done

    (** used for the other layers in a network *)
    let retropropagate la la' = 
      let v = (Matrix.transpose la'.w) *|/ la'.s in 
	for i=0 to dim la.s - 1 do
	  la.s.(i)<- (la.f' la.n.(i))*.v.(i)
	done
  end


module Network =
  struct
    type t = Layer.t array

    (** [make ne [|s1;s2;...;sn|]] creates a network with [ne] entries, 
	and n layers where layer i has [si] neurons *)
    let make ne ?(g = 0.1) t = 
      (Array.init (Array.length t) 
	(fun i ->
	  Layer.make 
	    (if i=0 then ne else t.(i-1))
	    t.(i) (Layer.sigmoid 0.4) ~g
	):t)

    (** [eval nt p] is the output of the network [nt] with the input [p] *)
    let eval (nt:t) p = Array.fold_left	(fun a la -> Layer.eval la a) p nt

    (** [learn nt p d] let the network [nt] learn that [p] should return [d] *)
    let learn nt p d =
      let len = Array.length nt in
	ignore (eval nt p);
	Layer.last_retropropagate nt.(len-1) d;
	for i=len-2 downto 0 do
	  Layer.retropropagate nt.(i) nt.(i+1);
	done; 
	Layer.update nt.(0) p;
	for i=1 to len - 1 do
	  Layer.update nt.(i) nt.(i-1).Layer.a
	done
	
    (** [training nt base n] trains the network [nt] with [base], [n] times *)
    let training nt base n =
      for i=0 to n-1 do
	for j=0 to Array.length base - 1 do
	  learn nt (fst base.(j)) (snd base.(j))
	done
      done


    (** [training nt base n] computes the distance between the last 
	computed value of [nt] with [d]*)
    let error_rate nt d =
      let len = Array.length nt in
      let d0 = nt.(len-1).Layer.a in
      let ds = d-/d0 in sqrt (ds*/ds)
	
      


    let set_of_bool = Array.map (function true -> 0.95 | false -> 0.05) (* should be better than 1. and 0.? *)
    let bool_of_set = Array.map ((<=) 0.5)
    let base_of_bool = Array.map set_of_bool

    let evalb nt p = bool_of_set (eval nt (set_of_bool p))
    let learnb nt p d = learn nt (set_of_bool p) (set_of_bool d)
    let trainingb nt base n =
      let b = Array.map (fun (i,j) -> set_of_bool i,set_of_bool j) base in
	training nt b n

	  
  end


