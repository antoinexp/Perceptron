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

(** perceptron.mli *)

module Layer :
  sig

    type t = {
      w : Math.Matrix.t;  (** w.(id neuron).(id weigh) *)
      b : Math.Matrix.Vector.t;  (** b.(id neuron) bias coeff *)
      s : Math.Matrix.Vector.t;  (** s.(id neuron) last computed sensibility *)
      n : Math.Matrix.Vector.t;  (** n.(id neuron) last computed output *)
      a : Math.Matrix.Vector.t;  (** a.(id neuron) = f n.(id neuron)*)
      f : float -> float; (** activation function (just below) *)
      f' : float -> float; (** derivative of the activation function *)
      g : float; (** learning rate *)
    }

    (** activation function *)
    val sigmoid : float -> (float -> float) * (float -> float)

    (** [make r s (f,f')] creates a layer of [s] neurons with [r] entries *)
    val make : int -> int -> ?g:float -> (float -> float) * (float -> float) -> t

    (** [eval la p] is the output of the layer [la] with [p] *)
    val eval : t -> Math.Matrix.Vector.t -> Math.Matrix.Vector.t

    (** [update la p] updates then layer [la] with the input [p] *)
    val update : t -> Math.Matrix.Vector.t -> unit

    (** used for the last layer in a network (see below) *)
    val last_retropropagate : t -> Math.Vector.t -> unit

    (** used for the other layers in a network *)
    val retropropagate : t -> t -> unit


  end


module Network :
  sig

    type t = Layer.t array

    (** [make ne [|s1;s2;...;sn|]] creates a network with [ne] entries, 
	and n layers where layer i has [si] neurons *)
    val make : int -> ?g:float -> int array -> t

   (** [eval nt p] is the output of the network [nt] with the input [p] *)
    val eval : t -> Math.Matrix.Vector.t -> Math.Matrix.Vector.t

   (** [learn nt p d] let the network [nt] learn that [p] should return [d] *)
    val learn : t -> Math.Matrix.Vector.t -> Math.Vector.t -> unit

    (** [training nt base n] trains the network [nt] with [base], [n] times *)
    val training : t -> (Math.Matrix.Vector.t * Math.Vector.t) array -> int -> unit

    (** [training nt base n] computes the distance between the last 
	computed value of [nt] with [d]*)
    val error_rate : t -> Math.Vector.t -> float

    (** {6 Usefull} *)

    val set_of_bool : bool array -> Math.Float.t array
    val bool_of_set : Math.Matrix.Vector.t -> bool array
    val base_of_bool : bool array array -> Math.Float.t array array
    val evalb : t -> bool array -> bool array
    val learnb : t -> bool array -> bool array -> unit
    val trainingb : t -> (bool array * bool array) array -> int -> unit


  end
