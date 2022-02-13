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

module Make (Elt : Sig.Type1) = struct
  module Elt = Elt
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

  let t2 x1 x2 = T2 (x1, x2)
  let t3 x1 x2 x3 = T3 (x1, x2, x3)
  let t4 x1 x2 x3 x4 = T4 (x1, x2, x3, x4)
  let t5 x1 x2 x3 x4 x5 = T5 (x1, x2, x3, x4, x5)
  let t6 x1 x2 x3 x4 x5 x6 = T6 (x1, x2, x3, x4, x5, x6)

  let to_tuple : type a. f: <call: 'b. 'b Elt.t -> 'b> -> a t -> a =
    fun ~f -> function
     | T2 (x1, x2) ->
        (f#call x1, f#call x2)
     | T3 (x1, x2, x3) ->
        (f#call x1, f#call x2, f#call x3)
     | T4 (x1, x2, x3, x4) ->
        (f#call x1, f#call x2, f#call x3, f#call x4)
     | T5 (x1, x2, x3, x4, x5) ->
        (f#call x1, f#call x2, f#call x3, f#call x4, f#call x5)
     | T6 (x1, x2, x3, x4, x5, x6) ->
        (f#call x1, f#call x2, f#call x3, f#call x4, f#call x5, f#call x6)

  let iter : type a. f: <call: 'b. 'b Elt.t -> unit> -> a t -> unit =
    fun ~f -> function
     | T2 (x1, x2) ->
        f#call x1; f#call x2
     | T3 (x1, x2, x3) ->
        f#call x1; f#call x2; f#call x3
     | T4 (x1, x2, x3, x4) ->
        f#call x1; f#call x2; f#call x3; f#call x4
     | T5 (x1, x2, x3, x4, x5) ->
        f#call x1; f#call x2; f#call x3; f#call x4; f#call x5
     | T6 (x1, x2, x3, x4, x5, x6) ->
        f#call x1; f#call x2; f#call x3; f#call x4; f#call x5; f#call x6

  let fold : type a. f: <call: 'c. 'c Elt.t -> 'b -> 'b> -> a t -> 'b -> 'b =
    fun ~f tup acc ->
    (match tup with
     | T2 (x1, x2) ->
        acc |> f#call x1 |> f#call x2
     | T3 (x1, x2, x3) ->
        acc |> f#call x1 |> f#call x2 |> f#call x3
     | T4 (x1, x2, x3, x4) ->
        acc |> f#call x1 |> f#call x2 |> f#call x3 |> f#call x4
     | T5 (x1, x2, x3, x4, x5) ->
        acc |> f#call x1 |> f#call x2 |> f#call x3 |> f#call x4
            |> f#call x5
     | T6 (x1, x2, x3, x4, x5, x6) ->
        acc |> f#call x1 |> f#call x2 |> f#call x3 |> f#call x4
            |> f#call x5 |> f#call x6)

  let for_all : type a. f: <call: 'b. 'b Elt.t -> bool> -> a t -> bool =
    fun ~f -> function
     | T2 (x1, x2) ->
        f#call x1 && f#call x2
     | T3 (x1, x2, x3) ->
        f#call x1 && f#call x2 && f#call x3
     | T4 (x1, x2, x3, x4) ->
        f#call x1 && f#call x2 && f#call x3 && f#call x4
     | T5 (x1, x2, x3, x4, x5) ->
        f#call x1 && f#call x2 && f#call x3 && f#call x4 &&
        f#call x5
     | T6 (x1, x2, x3, x4, x5, x6) ->
        f#call x1 && f#call x2 && f#call x3 && f#call x4 &&
        f#call x5 && f#call x6

  let exists : type a. f: <call: 'b. 'b Elt.t -> bool> -> a t -> bool =
    fun ~f -> function
     | T2 (x1, x2) ->
        f#call x1 || f#call x2
     | T3 (x1, x2, x3) ->
        f#call x1 || f#call x2 || f#call x3
     | T4 (x1, x2, x3, x4) ->
        f#call x1 || f#call x2 || f#call x3 || f#call x4
     | T5 (x1, x2, x3, x4, x5) ->
        f#call x1 || f#call x2 || f#call x3 || f#call x4 ||
        f#call x5
     | T6 (x1, x2, x3, x4, x5, x6) ->
        f#call x1 || f#call x2 || f#call x3 || f#call x4 ||
        f#call x5 || f#call x6

  let map : type a. f: <call: 'b. 'b Elt.t -> 'b Elt.t> -> a t -> a t =
    fun ~f -> function
     | T2 (x1, x2) ->
        T2 (f#call x1, f#call x2)
     | T3 (x1, x2, x3) ->
        T3 (f#call x1, f#call x2, f#call x3)
     | T4 (x1, x2, x3, x4) ->
        T4 (f#call x1, f#call x2, f#call x3, f#call x4)
     | T5 (x1, x2, x3, x4, x5) ->
        T5 (f#call x1, f#call x2, f#call x3, f#call x4, f#call x5)
     | T6 (x1, x2, x3, x4, x5, x6) ->
        T6 (f#call x1, f#call x2, f#call x3, f#call x4, f#call x5, f#call x6)

  let find_map :
        type a. f: <call: 'b. 'b Elt.t -> 'c option> -> a t -> 'c option =
    fun ~f -> function
     | T2 (x1, x2) ->
        begin
          match f#call x1 with Some _ as y -> y | None ->
          f#call x2
        end
     | T3 (x1, x2, x3) ->
        begin
          match f#call x1 with Some _ as y -> y | None ->
          match f#call x2 with Some _ as y -> y | None ->
          f#call x3
        end
     | T4 (x1, x2, x3, x4) ->
        begin
          match f#call x1 with Some _ as y -> y | None ->
          match f#call x2 with Some _ as y -> y | None ->
          match f#call x3 with Some _ as y -> y | None ->
          f#call x4
        end
     | T5 (x1, x2, x3, x4, x5) ->
        begin
          match f#call x1 with Some _ as y -> y | None ->
          match f#call x2 with Some _ as y -> y | None ->
          match f#call x3 with Some _ as y -> y | None ->
          match f#call x4 with Some _ as y -> y | None ->
          f#call x5
        end
     | T6 (x1, x2, x3, x4, x5, x6) ->
        begin
          match f#call x1 with Some _ as y -> y | None ->
          match f#call x2 with Some _ as y -> y | None ->
          match f#call x3 with Some _ as y -> y | None ->
          match f#call x4 with Some _ as y -> y | None ->
          match f#call x5 with Some _ as y -> y | None ->
          f#call x6
        end

  let iteri ~(f : <call: 'b. int -> 'b Elt.t -> unit>) tup =
    let i = ref 0 in
    let f' = object
      method call : 'b. 'b Elt.t -> unit = fun x -> f#call !i x; incr i
    end in
    iter ~f:f' tup

  let pp ~(f : <call: 'b. Format.formatter -> 'b Elt.t -> unit>) ppf tup =
    Format.fprintf ppf "(@[";
    let f' = object
      method call : 'b. int -> 'b Elt.t -> unit = fun i x ->
        if i > 0 then Format.fprintf ppf ",@ ";
        f#call ppf x
    end in
    iteri ~f:f' tup;
    Format.fprintf ppf "@])"
end

module Product (A : Sig.S) (B : Sig.S) = struct

  include Make (struct type 'a t = 'a A.Elt.t * 'a B.Elt.t end)

  let cons : type a. a A.t -> a B.t -> a t =
    fun a b -> match a, b with
     | T2 (x1, x2), T2 (y1, y2) ->
        T2 ((x1, y1), (x2, y2))
     | T3 (x1, x2, x3), T3 (y1, y2, y3) ->
        T3 ((x1, y1), (x2, y2), (x3, y3))
     | T4 (x1, x2, x3, x4), T4 (y1, y2, y3, y4) ->
        T4 ((x1, y1), (x2, y2), (x3, y3), (x4, y4))
     | T5 (x1, x2, x3, x4, x5), T5 (y1, y2, y3, y4, y5) ->
        T5 ((x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5))
     | T6 (x1, x2, x3, x4, x5, x6), T6 (y1, y2, y3, y4, y5, y6) ->
        T6 ((x1, y1), (x2, y2), (x3, y3), (x4, y4), (x5, y5), (x6, y6))

  let fst : type a. a t -> a A.t = function
   | T2 ((x1, _), (x2, _)) ->
      T2 (x1, x2)
   | T3 ((x1, _), (x2, _), (x3, _)) ->
      T3 (x1, x2, x3)
   | T4 ((x1, _), (x2, _), (x3, _), (x4, _)) ->
      T4 (x1, x2, x3, x4)
   | T5 ((x1, _), (x2, _), (x3, _), (x4, _), (x5, _)) ->
      T5 (x1, x2, x3, x4, x5)
   | T6 ((x1, _), (x2, _), (x3, _), (x4, _), (x5, _), (x6, _)) ->
      T6 (x1, x2, x3, x4, x5, x6)

  let snd : type a. a t -> a B.t = function
   | T2 ((_, y1), (_, y2)) ->
      T2 (y1, y2)
   | T3 ((_, y1), (_, y2), (_, y3)) ->
      T3 (y1, y2, y3)
   | T4 ((_, y1), (_, y2), (_, y3), (_, y4)) ->
      T4 (y1, y2, y3, y4)
   | T5 ((_, y1), (_, y2), (_, y3), (_, y4), (_, y5)) ->
      T5 (y1, y2, y3, y4, y5)
   | T6 ((_, y1), (_, y2), (_, y3), (_, y4), (_, y5), (_, y6)) ->
      T6 (y1, y2, y3, y4, y5, y6)
end

module Mapping (A : Sig.S) (B : Sig.S) = struct

  let of_tuple :
        type a. f: <call: 'b. 'b A.Elt.t -> 'b -> 'b B.Elt.t> ->
        a A.t -> a -> a B.t =
    fun ~f x y ->
    match x, y with
     | T2 (x1, x2), (y1, y2) ->
        T2 (f#call x1 y1, f#call x2 y2)
     | T3 (x1, x2, x3), (y1, y2, y3) ->
        T3 (f#call x1 y1, f#call x2 y2, f#call x3 y3)
     | T4 (x1, x2, x3, x4), (y1, y2, y3, y4) ->
        T4 (f#call x1 y1, f#call x2 y2, f#call x3 y3, f#call x4 y4)
     | T5 (x1, x2, x3, x4, x5), (y1, y2, y3, y4, y5) ->
        T5 (f#call x1 y1, f#call x2 y2, f#call x3 y3, f#call x4 y4,
            f#call x5 y5)
     | T6 (x1, x2, x3, x4, x5, x6), (y1, y2, y3, y4, y5, y6) ->
        T6 (f#call x1 y1, f#call x2 y2, f#call x3 y3, f#call x4 y4,
            f#call x5 y5, f#call x6 y6)

  let map : type a. f: <call: 'b. 'b A.Elt.t -> 'b B.Elt.t> -> a A.t -> a B.t =
    fun ~f -> function
     | T2 (x1, x2) ->
        T2 (f#call x1, f#call x2)
     | T3 (x1, x2, x3) ->
        T3 (f#call x1, f#call x2, f#call x3)
     | T4 (x1, x2, x3, x4) ->
        T4 (f#call x1, f#call x2, f#call x3, f#call x4)
     | T5 (x1, x2, x3, x4, x5) ->
        T5 (f#call x1, f#call x2, f#call x3, f#call x4, f#call x5)
     | T6 (x1, x2, x3, x4, x5, x6) ->
        T6 (f#call x1, f#call x2, f#call x3, f#call x4, f#call x5, f#call x6)
end

module Endoconstructors
  (A : Sig.S) (F : sig val cons : 'a A.t -> 'a A.Elt.t end) =
struct
  let t2 x1 x2 = F.cons (A.t2 x1 x2)
  let t3 x1 x2 x3 = F.cons (A.t3 x1 x2 x3)
  let t4 x1 x2 x3 x4 = F.cons (A.t4 x1 x2 x3 x4)
  let t5 x1 x2 x3 x4 x5 = F.cons (A.t5 x1 x2 x3 x4 x5)
  let t6 x1 x2 x3 x4 x5 x6 = F.cons (A.t6 x1 x2 x3 x4 x5 x6)
end
