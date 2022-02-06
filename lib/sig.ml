(* Copyright (C) 2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

module type Type1 = sig
  type 'a t
end

module type Constructors = sig
  type 'a elt
  type 'a t

  val t2 : 'a elt -> 'b elt -> ('a * 'b) t
  val t3 : 'a elt -> 'b elt -> 'c elt -> ('a * 'b * 'c) t
  val t4 : 'a elt -> 'b elt -> 'c elt -> 'd elt -> ('a * 'b * 'c * 'd) t
  val t5 : 'a elt -> 'b elt -> 'c elt -> 'd elt -> 'e elt ->
    ('a * 'b * 'c * 'd * 'e) t
  val t6 : 'a elt -> 'b elt -> 'c elt -> 'd elt -> 'e elt -> 'f elt ->
    ('a * 'b * 'c * 'd * 'e * 'f) t
end

module type Abstract = sig
  type 'a elt
  type 'a t
end

module type S = sig
  module Elt : Type1

  type _ t =
    | T2 : 'a Elt.t * 'b Elt.t ->
        ('a * 'b) t
    | T3 : 'a Elt.t * 'b Elt.t * 'c Elt.t ->
        ('a * 'b * 'c) t
    | T4 : 'a Elt.t * 'b Elt.t * 'c Elt.t * 'd Elt.t ->
        ('a * 'b * 'c * 'd) t
    | T5 : 'a Elt.t * 'b Elt.t * 'c Elt.t * 'd Elt.t * 'e Elt.t ->
        ('a * 'b * 'c * 'd * 'e) t
    | T6 : 'a Elt.t * 'b Elt.t * 'c Elt.t * 'd Elt.t * 'e Elt.t * 'f Elt.t ->
        ('a * 'b * 'c * 'd * 'e * 'f) t

  include Constructors with type 'a elt := 'a Elt.t and type 'a t := 'a t

  val to_tuple : f: <call: 'b. 'b Elt.t -> 'b> -> 'a t -> 'a

  val iter : f: <call: 'b. 'b Elt.t -> unit> -> 'a t -> unit

  val fold : f: <call: 'c. 'c Elt.t -> 'b -> 'b> -> 'a t -> 'b -> 'b

  val for_all : f: <call: 'b. 'b Elt.t -> bool> -> 'a t -> bool

  val exists : f: <call: 'b. 'b Elt.t -> bool> -> 'a t -> bool

  val map : f: <call: 'b. 'b Elt.t -> 'b Elt.t> -> 'a t -> 'a t

  val find_map : f: <call: 'b. 'b Elt.t -> 'c option> -> 'a t -> 'c option

  val pp : f: <call: 'b. Format.formatter -> 'b Elt.t -> unit> ->
    Format.formatter -> 'a t -> unit
end
