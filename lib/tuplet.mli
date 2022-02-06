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

module Sig = Sig

module Make (Elt : Sig.Type1) : Sig.S with module Elt = Elt

module Product (A : Sig.S) (B : Sig.S) : sig
  include Sig.S with type 'a Elt.t = 'a A.Elt.t * 'a B.Elt.t

  val cons : 'a A.t -> 'a B.t -> 'a t

  val fst : 'a t -> 'a A.t

  val snd : 'a t -> 'a B.t
end

module Mapping (A : Sig.S) (B : Sig.S) : sig

  val of_tuple : <call: 'b. 'b A.Elt.t -> 'b -> 'b B.Elt.t> ->
    'a A.t -> 'a -> 'a B.t

  val map : <call: 'b. 'b A.Elt.t -> 'b B.Elt.t> ->
    'a A.t -> 'a B.t

end

module Endoconstructors :
  functor (A : Sig.S) ->
  functor (F : sig val cons : 'a A.t -> 'a A.Elt.t end) ->
  Sig.Constructors with type 'a elt := 'a A.Elt.t and type 'a t := 'a A.Elt.t
