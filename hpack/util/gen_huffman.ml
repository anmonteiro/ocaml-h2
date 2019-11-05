(*----------------------------------------------------------------------------
  Copyright (c) 2019 António Nuno Monteiro

  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

  1. Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  2. Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

  3. Neither the name of the copyright holder nor the names of its contributors
  may be used to endorse or promote products derived from this software without
  specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Parsetree
open Ast_helper
open Asttypes

let let_ name body =
  Str.value
    Nonrecursive
    [ Vb.mk (Pat.var { txt = name; loc = !default_loc }) body ]

type node =
  { mutable id : int
  ; mutable accept : bool
  ; left : child
  ; right : child
  ; transitions : (int option * bool * char option) array
  }

and child =
  | Node of node
  | Symbol of char
  | Missing

let make_node ?(left = Missing) ?(right = Missing) () =
  { id = 0
  ; accept = false
  ; left
  ; right
  ; transitions = Array.make 16 (None, false, None)
  }

let rec add_symbol tree symbol = function
  | [] ->
    Symbol symbol
  | false :: bits ->
    (match tree with
    | Missing ->
      Node (make_node ~left:(add_symbol tree symbol bits) ())
    | Node node ->
      Node { node with left = add_symbol node.left symbol bits }
    | Symbol _ ->
      failwith "add_symbol")
  | true :: bits ->
    (match tree with
    | Missing ->
      Node (make_node ~right:(add_symbol tree symbol bits) ())
    | Node node ->
      Node { node with right = add_symbol node.right symbol bits }
    | Symbol _ ->
      failwith "add_symbol")

let rec set_ids tree eos next_id =
  match tree with
  | Node node ->
    node.id <- next_id;
    if eos < 8 then node.accept <- true;
    next_id + 1 |> set_ids node.left 8 |> set_ids node.right (eos + 1)
  | _ ->
    next_id

let rec traverse root transitions failed symbol node remaining i =
  let failed, node, symbol =
    match node with
    | Symbol symbol ->
      failed, root, Some symbol
    | Node node ->
      failed, node, symbol
    | Missing ->
      true, root, None
  in
  if remaining = 0 then (
    transitions.(i) <-
      (if failed then
         None, false, None
      else
        Some node.id, node.accept, symbol);
    i + 1)
  else
    traverse root transitions failed symbol node.left (remaining - 1) i
    |> traverse root transitions failed symbol node.right (remaining - 1)

let rec make_transitions root = function
  | Node node ->
    let i = traverse root node.transitions false None (Node node) 4 0 in
    assert (i = 16);
    make_transitions root node.left;
    make_transitions root node.right
  | _ ->
    ()

let mk_encode_table encode_table =
  let items =
    Array.fold_left
      (fun acc (code, length) ->
        let tup =
          Exp.tuple
            [ Exp.constant (Pconst_integer (string_of_int code, None))
            ; Exp.constant (Pconst_integer (string_of_int length, None))
            ]
        in
        tup :: acc)
      []
      encode_table
  in
  let_ "encode_table" (Exp.array (List.rev items))

let output_transition (id, accept, symbol) =
  let output_int = function
    | Some i ->
      Exp.constant (Pconst_integer (string_of_int i, None))
    | None ->
      Exp.constant (Pconst_integer ("-1", None))
  in
  let output_bool b =
    Exp.construct
      { txt = Longident.Lident (if b then "true" else "false")
      ; loc = !default_loc
      }
      None
  in
  let output_char = function
    | Some c ->
      Exp.constant (Pconst_char c)
    | None ->
      Exp.constant (Pconst_char '\000')
  in
  Exp.tuple [ output_int id; output_bool accept; output_char symbol ]

let mk_decode_table tree =
  let rec loop tree (i, acc) =
    match tree with
    | Node node ->
      assert (node.id = i);
      let acc' =
        Array.fold_left
          (fun acc transition -> output_transition transition :: acc)
          acc
          node.transitions
      in
      (i + 1, acc') |> loop node.left |> loop node.right
    | _ ->
      i, acc
  in
  let i, items = loop tree (0, []) in
  assert (i = 256);
  let_ "decode_table" (Exp.array (List.rev items))

let bits_of_string s =
  let rec aux i =
    if i < String.length s then
      match s.[i] with
      | '|' ->
        aux (i + 1)
      | '0' ->
        false :: aux (i + 1)
      | '1' ->
        true :: aux (i + 1)
      | _ ->
        failwith "bits_of_string"
    else
      []
  in
  aux 0

let () =
  let ic = Scanf.Scanning.from_file Sys.argv.(1) in
  let encode_table = Array.make 256 (0, 0) in
  let rec loop tree i =
    if i < 256 then (
      Scanf.bscanf ic "%_c%_c%_c ( %d ) %s %x [ %d ]\n"
      @@ fun _i s code length ->
      assert (i = _i);
      encode_table.(i) <- code, length;
      let tree = add_symbol tree (char_of_int i) (bits_of_string s) in
      loop tree (i + 1))
    else
      let ids = set_ids tree 0 0 in
      assert (ids = 256);
      tree
  in
  let tree = loop Missing 0 in
  let root = match tree with Node node -> node | _ -> failwith "empty tree" in
  make_transitions root tree;
  let ppf = Format.std_formatter in
  Format.fprintf ppf "(* generated by util/gen_huffman.ml *)\n\n";
  Pprintast.structure ppf [ mk_encode_table encode_table; mk_decode_table tree ]
