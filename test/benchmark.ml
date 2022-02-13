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

module Tup = Tuplet.Make (struct type 'a t = int end)
type any_tup = Any_tup : 'a Tup.t -> any_tup

let cases = [|
  Any_tup (Tup.t2 1 2);
  Any_tup (Tup.t3 1 2 3);
  Any_tup (Tup.t4 1 2 3 4);
  Any_tup (Tup.t5 1 2 3 4 5);
  Any_tup (Tup.t6 1 2 3 4 5 6);
  Any_tup (Tup.t7 1 2 3 4 5 6 7);
  Any_tup (Tup.t8 1 2 3 4 5 6 7 8);
|]

let test_iter r =
  let Any_tup tup = cases.(r - 2) in
  Bechamel.Staged.stage @@ fun () ->
  let sum = ref 0 in
  Tup.iter tup ~f:(object
    method call i = sum := !sum + i
  end)

let test_fold r =
  let Any_tup tup = cases.(r - 2) in
  Bechamel.Staged.stage @@ fun () ->
  ignore @@ Tup.fold tup 0 ~f:(object
    method call i acc = acc + i
  end)

let test_exists r =
  let j = Random.int (r + 1) in
  let Any_tup tup = cases.(r - 2) in
  Bechamel.Staged.stage @@ fun () ->
  ignore @@ Tup.exists tup ~f:(object
    method call i = i = j
  end)

let test_map r =
  let Any_tup tup = cases.(r - 2) in
  Bechamel.Staged.stage @@ fun () ->
  ignore @@ Tup.map tup ~f:(object
    method call = succ
  end)

let test_find_map r =
  let j = Random.int (r + 1) in
  let Any_tup tup = cases.(r - 2) in
  Bechamel.Staged.stage @@ fun () ->
  ignore @@ Tup.find_map tup ~f:(object
    method call i = if i = j then Some () else None
  end)

let tests =
  let open Bechamel.Test in
  let args = [2; 3; 4; 5; 6; 7; 8] in
  make_grouped ~name:"tuplet" [
    make_indexed ~name:"iter" ~args test_iter;
    make_indexed ~name:"fold" ~args test_fold;
    make_indexed ~name:"exists" ~args test_exists;
    make_indexed ~name:"map" ~args test_map;
    make_indexed ~name:"find_map" ~args test_find_map;
  ]

let instances = Bechamel.Toolkit.Instance.[
  minor_allocated;
  major_allocated;
  monotonic_clock;
]

let benchmark () =
  let open Bechamel in
  let ols =
    Analyze.ols ~bootstrap:0 ~r_square:true ~predictors:Measure.[|run|]
  in
  let cfg =
    Benchmark.cfg ~limit:2000 ~quota:(Time.second 2.0) ~kde:(Some 1000) ()
  in
  let raw_results = Benchmark.all cfg instances tests in
  let results =
    List.map (fun instance -> Analyze.all ols instance raw_results) instances
  in
  Analyze.merge ols instances results

let () =
  List.iter (fun v -> Bechamel_notty.Unit.add v (Bechamel.Measure.unit v))
    instances;
  let window =
    (match Notty_unix.winsize Unix.stdout with
     | Some (w, h) -> {Bechamel_notty.w; h}
     | None -> {Bechamel_notty.w = 80; h = 1})
  in
  benchmark ()
    |> Bechamel_notty.Multiple.image_of_ols_results
        ~rect:window
        ~predictor:Bechamel.Measure.run
    |> Notty_unix.eol
    |> Notty_unix.output_image
