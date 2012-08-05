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

(** math.mli *)

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
module GVector :
  functor (K : Field) ->
    sig
      type t = K.t array
      val make : int -> K.t -> t
      val init : int -> (int -> K.t) -> t
      val zero : int -> t
      val e : int -> int -> t
      val copy : t -> t
      val dim : t -> int
      val op : (K.t -> K.t -> K.t) -> t -> t -> t
      val ( +/ ) : t -> t -> t
      val ( -/ ) : t -> t -> t
      val ( */ ) : t -> t -> K.t
      val ( /* ) : K.t -> K.t array -> t
      val set : 'a array -> 'a array -> unit
      val print : float array -> unit
    end
module GMatrix :
  functor (K : Field) ->
    sig
      type t = K.t array array
      module Vector :
        sig
          type t = K.t array
          val make : int -> K.t -> t
          val init : int -> (int -> K.t) -> t
          val zero : int -> t
          val e : int -> int -> t
          val copy : t -> t
          val dim : t -> int
          val op : (K.t -> K.t -> K.t) -> t -> t -> t
          val ( +/ ) : t -> t -> t
          val ( -/ ) : t -> t -> t
          val ( */ ) : t -> t -> K.t
          val ( /* ) : K.t -> K.t array -> t
          val set : 'a array -> 'a array -> unit
          val print : float array -> unit
        end
      val make : int -> int -> K.t -> t
      val init : int -> int -> (int -> int -> K.t) -> t
      val zero : int -> int -> t
      val one : int -> t
      val identity : int -> t
      val diag : int -> (int -> K.t) -> t
      val line : t -> int
      val row : t -> int
      val op : (K.t -> K.t -> K.t) -> t -> t -> t
      val ( +| ) : t -> t -> t
      val ( -| ) : t -> t -> t
      val ( *| ) : t -> t -> t
      val ( *|/ ) : t -> Vector.t -> Vector.t
      val transpose : t -> t
      val of_vector : Vector.t -> t
      val to_line : Vector.t -> t
      val set : t -> K.t array array -> unit
      val print : t -> unit
      val ( |* ) : K.t -> t -> t
    end
module Float :
  sig
    type t = float
    val ( + ) : float -> float -> float
    val ( * ) : float -> float -> float
    val ( - ) : float -> float -> float
    val ( / ) : float -> float -> float
    val zero : float
    val one : float
    val print : float -> unit
  end
module Vector :
  sig
    type t = Float.t array
    val make : int -> Float.t -> t
    val init : int -> (int -> Float.t) -> t
    val zero : int -> t
    val e : int -> int -> t
    val copy : t -> t
    val dim : t -> int
    val op : (Float.t -> Float.t -> Float.t) -> t -> t -> t
    val ( +/ ) : t -> t -> t
    val ( -/ ) : t -> t -> t
    val ( */ ) : t -> t -> Float.t
    val ( /* ) : Float.t -> Float.t array -> t
    val set : 'a array -> 'a array -> unit
    val print : float array -> unit
  end
module Matrix :
  sig
    type t = Float.t array array
    module Vector :
      sig
        type t = Float.t array
        val make : int -> Float.t -> t
        val init : int -> (int -> Float.t) -> t
        val zero : int -> t
        val e : int -> int -> t
        val copy : t -> t
        val dim : t -> int
        val op : (Float.t -> Float.t -> Float.t) -> t -> t -> t
        val ( +/ ) : t -> t -> t
        val ( -/ ) : t -> t -> t
        val ( */ ) : t -> t -> Float.t
        val ( /* ) : Float.t -> Float.t array -> t
        val set : 'a array -> 'a array -> unit
        val print : float array -> unit
      end
    val make : int -> int -> Float.t -> t
    val init : int -> int -> (int -> int -> Float.t) -> t
    val zero : int -> int -> t
    val one : int -> t
    val identity : int -> t
    val diag : int -> (int -> Float.t) -> t
    val line : t -> int
    val row : t -> int
    val op : (Float.t -> Float.t -> Float.t) -> t -> t -> t
    val ( +| ) : t -> t -> t
    val ( -| ) : t -> t -> t
    val ( *| ) : t -> t -> t
    val ( *|/ ) : t -> Vector.t -> Vector.t
    val transpose : t -> t
    val of_vector : Vector.t -> t
    val to_line : Vector.t -> t
    val set : t -> Float.t array array -> unit
    val print : t -> unit
    val ( |* ) : Float.t -> t -> t
  end
