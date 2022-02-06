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

let (%) f g x = f (g x)

module type TYPE = sig

  module Tup : Tuplet.Sig.S

  type _ t =
    | Unit : unit t
    | Int : int t
    | Array : 'a t -> 'a array t
    | Tuple : 'a Tup.t -> 'a t
    | Func : 'a t * 'b t -> ('a -> 'b) t

  include Tuplet.Sig.Constructors with type 'a elt := 'a t and type 'a t := 'a t

  val pp : 'a t Fmt.t

end

module rec Type : TYPE with type 'a Tup.Elt.t = 'a Type.t = struct

  module Tup = Tuplet.Make (Type)

  type _ t =
    | Unit : unit t
    | Int : int t
    | Array : 'a t -> 'a array t
    | Tuple : 'a Tup.t -> 'a t
    | Func : 'a t * 'b t -> ('a -> 'b) t

  include Tuplet.Endoconstructors (Tup) (struct let cons = fun x -> Tuple x end)

  let rec pp : type a. a t Fmt.t = fun ppf -> function
   | Unit -> Fmt.string ppf "unit"
   | Int -> Fmt.string ppf "int"
   | Array t -> Fmt.(pp ++ sp ++ const string "array") ppf t
   | Tuple u ->
      let pp' = object
        method call : 'b. 'b Type.t Fmt.t = pp
      end in
      Tup.pp pp' ppf u
   | Func (a, b) ->
      Fmt.(using fst pp ++ sp ++ const string "->" ++ sp ++ using snd pp)
        ppf (a, b)

end

module type VALUE = sig

  module Tup : Tuplet.Sig.S

  type _ t =
    | Unit : unit t
    | Int : int -> int t
    | Array : 'a Type.t * 'a t array -> 'a array t
    | Tuple : 'a Tup.t -> 'a t
    | Lambda : ('a -> 'b) Type.t * ('a t -> 'b t) -> ('a -> 'b) t

  include Tuplet.Sig.Constructors with type 'a elt := 'a t and type 'a t := 'a t

  val typeof : 'a t -> 'a Type.t

  val of_tuple : 'a Type.t -> 'a -> 'a t

  val to_tuple : 'a t -> 'a

  val equal : 'a t -> 'a t -> bool

  val pp : Format.formatter -> 'a t -> unit

end

module rec Value : VALUE with type 'a Tup.Elt.t = 'a Value.t = struct

  module Tup = Tuplet.Make (Value)
  module Tup2 = Tuplet.Product (Tup) (Tup)
  module MapTV = Tuplet.Mapping (Type.Tup) (Tup)
  module MapVT = Tuplet.Mapping (Tup) (Type.Tup)

  type _ t =
    | Unit : unit t
    | Int : int -> int t
    | Array : 'a Type.t * 'a t array -> 'a array t
    | Tuple : 'a Tup.t -> 'a t
    | Lambda : ('a -> 'b) Type.t * ('a t -> 'b t) -> ('a -> 'b) t

  include Tuplet.Endoconstructors (Tup) (struct let cons = fun x -> Tuple x end)

  let rec typeof : type a. a t -> a Type.t = function
   | Unit -> Type.Unit
   | Int _ -> Type.Int
   | Array (t, _) -> Type.Array t
   | Tuple u ->
      let typeof' = object
        method call : 'b. 'b t -> 'b Type.t = typeof
      end in
      Tuple (MapVT.map typeof' u)
   | Lambda (t, _) -> t

  let rec of_tuple : type a. a Type.t -> a -> a t =
    fun t v ->
    (match t, v with
     | Type.Unit, () -> Unit
     | Type.Int, i -> Int i
     | Type.Array t, xs -> Array (t, Array.map (of_tuple t) xs)
     | Type.Tuple t, v ->
        let of_tuple' = object
          method call : 'b. 'b Type.t -> 'b -> 'b t = of_tuple
        end in
        Tuple (MapTV.of_tuple of_tuple' t v)
     | Type.Func (_, b), f ->
        let f' = of_tuple b % f % to_tuple in
        Lambda (t, f'))
  and to_tuple : type a. a t -> a = function
   | Unit -> ()
   | Int i -> i
   | Array (_, xs) -> Array.map to_tuple xs
   | Tuple u ->
      let to_tuple' = object
        method call : 'b. 'b t -> 'b = to_tuple
      end in
      Tup.to_tuple to_tuple' u
   | Lambda (Type.Func (a, _), f) ->
      to_tuple % f % of_tuple a
   | _ -> .

  let rec equal : type a. a t -> a t -> bool = fun x y ->
    (match x, y with
     | Unit, Unit -> true
     | Int i, Int j -> i = j
     | Array (_, xs), Array (_, ys) -> Array.for_all2 equal xs ys
     | Tuple u, Tuple v ->
        let equal' = object
          method call : 'b. 'b t * 'b t -> bool = fun (x, y) -> equal x y
        end in
        Tup2.for_all equal' (Tup2.cons u v)
     | Lambda (_, f), Lambda (_, g) -> f == g
     | _ -> .)

  let rec pp : type a. Format.formatter -> a t -> unit = fun ppf -> function
   | Unit -> Format.pp_print_string ppf "()"
   | Int i -> Format.pp_print_int ppf i
   | Array (_, xs) -> Fmt.(brackets @@ array ~sep:comma pp) ppf xs
   | Tuple tup ->
      let pp = object
        method call : 'b. Format.formatter -> 'b t -> unit = pp
      end in
      Tup.pp pp ppf tup
   | Lambda (_, _) -> Fmt.(const string "<lambda>") ppf ()
end

let () =
  let v1 =
    let open Value in
    t3 (Int 0) (Array (Type.Int, [|Int 2; Int 3; Int 5|])) (t2 Unit (Int 1))
  in
  let x1 = Value.to_tuple v1 in
  let t1 = Value.typeof v1 in
  let x2 = (0, [|2; 3; 5|], ((), 1)) in
  let v2 = Value.of_tuple t1 x2 in
  assert (Value.equal v1 v2);
  assert (x1 = x2);
  Format.printf "%a : %a\n" Value.pp v1 Type.pp t1
