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

let (%>) f g x = g (f x)
let (|?) x y = match x with Some _ as z -> z | None -> y ()

module Make (Elt : Sig.Type1) = struct
  module Elt = Elt
  type _ t =
    | T2 : 'a Elt.t * 'b Elt.t ->
        ('a * 'b) t
    | T3 : 'a Elt.t * 'b Elt.t * 'c Elt.t ->
        ('a * 'b * 'c) t
(*
    | T4 : 'a Elt.t * 'b Elt.t * 'c Elt.t * 'd Elt.t ->
        ('a * 'b * 'c * 'd) t
    | T5 : 'a Elt.t * 'b Elt.t * 'c Elt.t * 'd Elt.t * 'e Elt.t ->
        ('a * 'b * 'c * 'd * 'e) t
    | T6 : 'a Elt.t * 'b Elt.t * 'c Elt.t * 'd Elt.t * 'e Elt.t * 'f Elt.t ->
        ('a * 'b * 'c * 'd * 'e * 'f) t
*)

  let t2 x1 x2 = T2 (x1, x2)
  let t3 x1 x2 x3 = T3 (x1, x2, x3)

  let to_tuple : type a. <call: 'b. 'b Elt.t -> 'b> -> a t -> a =
    fun f -> function
     | T2 (x1, x2) ->
        (f#call x1, f#call x2)
     | T3 (x1, x2, x3) ->
        (f#call x1, f#call x2, f#call x3)

  let iter : type a. <call: 'b. 'b Elt.t -> unit> -> a t -> unit =
    fun f -> function
     | T2 (x1, x2) ->
        f#call x1; f#call x2
     | T3 (x1, x2, x3) ->
        f#call x1; f#call x2; f#call x3

  let fold : type a. <call: 'c. 'c Elt.t -> 'b -> 'b> -> a t -> 'b -> 'b =
    fun f -> function
     | T2 (x1, x2) ->
        f#call x1 %> f#call x2
     | T3 (x1, x2, x3) ->
        f#call x1 %> f#call x2 %> f#call x3

  let for_all : type a. <call: 'b. 'b Elt.t -> bool> -> a t -> bool =
    fun f -> function
     | T2 (x1, x2) ->
        f#call x1 && f#call x2
     | T3 (x1, x2, x3) ->
        f#call x1 && f#call x2 && f#call x3

  let exists : type a. <call: 'b. 'b Elt.t -> bool> -> a t -> bool =
    fun f -> function
     | T2 (x1, x2) ->
        f#call x1 || f#call x2
     | T3 (x1, x2, x3) ->
        f#call x1 || f#call x2 || f#call x3

  let map : type a. <call: 'b. 'b Elt.t -> 'b Elt.t> -> a t -> a t =
    fun f -> function
     | T2 (x1, x2) ->
        T2 (f#call x1, f#call x2)
     | T3 (x1, x2, x3) ->
        T3 (f#call x1, f#call x2, f#call x3)

  let find_map : type a. <call: 'b. 'b Elt.t -> 'c option> -> a t -> 'c option =
    fun f -> function
     | T2 (x1, x2) ->
        f#call x1 |? fun () -> f#call x2
     | T3 (x1, x2, x3) ->
        f#call x1 |? fun () -> f#call x2 |? fun () -> f#call x3

  let iteri (f : <call: 'b. int -> 'b Elt.t -> unit>) tup =
    let i = ref 0 in
    let f' = object
      method call : 'b. 'b Elt.t -> unit = fun x -> f#call !i x; incr i
    end in
    iter f' tup

  let pp (f : <call: 'b. Format.formatter -> 'b Elt.t -> unit>) ppf tup =
    Format.fprintf ppf "(@[";
    let f' = object
      method call : 'b. int -> 'b Elt.t -> unit = fun i x ->
        if i > 0 then Format.fprintf ppf ",@ ";
        f#call ppf x
    end in
    iteri f' tup;
    Format.fprintf ppf "@])"
end

module Product (A : Sig.S) (B : Sig.S) = struct

  include Make (struct type 'a t = 'a A.Elt.t * 'a B.Elt.t end)

  let cons : type a. a A.t -> a B.t -> a t =
    fun a b ->
    (match a, b with
     | T2 (x1, x2), T2 (y1, y2) ->
        T2 ((x1, y1), (x2, y2))
     | T3 (x1, x2, x3), T3 (y1, y2, y3) ->
        T3 ((x1, y1), (x2, y2), (x3, y3)))

  let fst : type a. a t -> a A.t = function
   | T2 ((x1, _), (x2, _)) ->
      T2 (x1, x2)
   | T3 ((x1, _), (x2, _), (x3, _)) ->
      T3 (x1, x2, x3)

  let snd : type a. a t -> a B.t = function
   | T2 ((_, y1), (_, y2)) ->
      T2 (y1, y2)
   | T3 ((_, y1), (_, y2), (_, y3)) ->
      T3 (y1, y2, y3)
end

module Mapping (A : Sig.S) (B : Sig.S) = struct

  let of_tuple :
        type a. <call: 'b. 'b A.Elt.t -> 'b -> 'b B.Elt.t> ->
        a A.t -> a -> a B.t =
    fun f x y ->
    (match x, y with
     | T2 (x1, x2), (y1, y2) ->
        T2 (f#call x1 y1, f#call x2 y2)
     | T3 (x1, x2, x3), (y1, y2, y3) ->
        T3 (f#call x1 y1, f#call x2 y2, f#call x3 y3))

  let map : type a. <call: 'b. 'b A.Elt.t -> 'b B.Elt.t> -> a A.t -> a B.t =
    fun f -> function
     | T2 (x1, x2) ->
        T2 (f#call x1, f#call x2)
     | T3 (x1, x2, x3) ->
        T3 (f#call x1, f#call x2, f#call x3)
end

module Endoconstructors
  (A : Sig.S) (F : sig val cons : 'a A.t -> 'a A.Elt.t end) =
struct
  let t2 x1 x2 = F.cons (A.t2 x1 x2)
  let t3 x1 x2 x3 = F.cons (A.t3 x1 x2 x3)
end
