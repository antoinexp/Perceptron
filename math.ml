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

(** math.ml *)


module type Field =
  sig
    type t
    val ( + ) : t -> t -> t
    val ( * ) : t -> t -> t
    val ( - ) : t -> t -> t
    val ( / ) : t -> t -> t
    val zero : t
    val one : t
    val print : t -> unit
  end


module GVector (K:Field) =
  struct
    type t = K.t array
    let make n (v:K.t) = (Array.make n v:t)
    let init n (f:int->K.t) = (Array.init n f:t)
    let zero n = make n K.zero
    let e n i = let v = zero n in v.(i)<-K.one; v (* unused *)
    let (copy:t->t) = Array.copy
    let dim (x:t) = Array.length x
    let op f x y = init (min (dim x) (dim y)) (fun i -> f x.(i) y.(i))
    let ( +/ ) = op K.( + ) 
    let ( -/ ) = op K.( - ) 
    let ( */ ) x y = Array.fold_left K.( + ) K.zero (op K.( * ) x y) (* dot product *)
    let ( /* ) k x = (Array.map (K.( * ) k) x:t) (* extern binary operation *)
    let set x y = Array.iteri (Array.set x) y (* x becomes equal to y *)
    let print = Array.iter (Printf.printf "[%f]\n")
  end


module GMatrix (K:Field) =
  struct
    type t = K.t array array

    module Vector = GVector(K)

    (** [make n p v] creates a matrix with [n] lines, and [p] rows with value ([v]) *)
    let make n p (v:K.t) = (Array.make_matrix n p v:t)

    (** [init n p f] create a matrix with [n] lines, and [p] rows (f i j) *)
    let init n p (f:int->int->K.t) =
      (Array.init n (fun i -> Array.init p (f i)):t)

    (** [zero n p] creates a matrix filled up with zero *)
    let zero n p = make n p K.zero

    (** [one n] creates the identity *)
    let one n = 
      let m = zero n n in
	for i=0 to n-1 do
	  m.(i).(i)<-K.one
	done; m

    (** [identity n] same as {!GMatrix.one} *)
    let identity = one

    let diag n f =
      let m = zero n n in
	for i=0 to n-1 do
	  m.(i).(i)<-f i
	done; m

    let line (m:t) = Array.length m
    let row (m:t) = Array.length m.(0)

    let op f x y = 
      init (min (line x) (line y)) (min (row x) (row y)) 
	(fun i j -> f x.(i).(j) y.(i).(j))

    let ( +| ) = op K.( + )
    let ( -| ) = op K.( - )

    let ( *| ) x y =
      let n = row x in
	init (line x) (row y)
	  (fun i j ->
	    let r = ref K.zero in
	      for k=0 to n-1 do
		r:=K.( + ) (!r) (K.( * ) x.(i).(k) y.(k).(j))
	      done; (!r)
	  )

    let ( *|/ ) (m:t) (v:Vector.t) = 
      let n = line m in
	Vector.init n 
	  (fun i ->
	    let r = ref K.zero in
	      for k=0 to Vector.dim v-1 do
		r:=K.( + ) (!r) (K.( * ) m.(i).(k) v.(k))
	      done; (!r)
	  )

    let transpose m = init (row m) (line m) (fun i j -> m.(j).(i))

    (** [of_vector v] returns a matrix (column) from a vector *)
    let of_vector (x:Vector.t) = transpose [|x|]

    (** [to_line v] returns a matrix (line) from a vector *)
    let to_line (x:Vector.t) = ([|x|]:t)

    (** [set a b] replaces a by b *)
    let set x y =
      for i=0 to line x - 1 do
	for j=0 to row x-1 do
	  x.(i).(j)<-y.(i).(j)
	done
      done

    let print m =
      for i=0 to line m - 1 do
	print_string "[";
	for j=0 to row m - 1 do
	  K.print m.(i).(j);
	  print_string " "
	done;
	print_endline "]"
      done

    (** [k |* m] multiplies m by k *)
    let ( |* ) k x = 
      init (line x) (row x) 
	(fun i j -> K.( * ) k x.(i).(j))
  end



module Float =
  struct
    type t = float
    let ( + ) = ( +. )
    let ( * ) = ( *. )
    let ( - ) = ( -. )
    let ( / ) = ( /. )
    let zero = 0.
    let one = 1.
    let print = print_float
  end

module Vector = GVector(Float)
module Matrix = GMatrix(Float)
