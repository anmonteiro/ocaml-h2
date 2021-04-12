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

let ghloc = !default_loc

let let_ name body =
  Str.value Nonrecursive [ Vb.mk (Pat.var { txt = name; loc = ghloc }) body ]

module CharSet = Set.Make (Char)

module Hashtbl = struct
  include Hashtbl

  let[@inline] find_opt h key = try Some (find h key) with Not_found -> None
end

let token_of_name =
  String.map @@ function ('a' .. 'z' | 'A' .. 'Z') as c -> c | _ -> '_'

let mk_tokens static_table =
  let _, tokens =
    Array.fold_left
      (fun (prev_token, acc) (i, name, _) ->
        if name <> prev_token then
          let token =
            let_
              (Printf.sprintf "token_%s" (token_of_name name))
              (Exp.constant (Pconst_integer (string_of_int i, None)))
          in
          name, token :: acc
        else
          name, acc)
      ("", [])
      static_table
  in
  List.rev tokens

let add_name name i names =
  let new_val =
    match Hashtbl.find_opt names name with Some i' -> min i i' | None -> i
  in
  Hashtbl.replace names name new_val

let find_pos names =
  let n = Hashtbl.length names in
  let names = Hashtbl.fold (fun k _ lst -> k :: lst) names [] in
  let rec loop pos =
    if
      List.map (fun name -> name.[pos]) names
      |> CharSet.of_list
      |> CharSet.cardinal
      |> ( = ) n
    then
      pos
    else
      loop (pos + 1)
  in
  loop 0

let make_token_map static_table =
  let tbl = Hashtbl.create 60 in
  Array.iter
    (fun (i, name, _) ->
      let length = String.length name in
      let string_tbl =
        match Hashtbl.find_opt tbl length with
        | Some string_tbl ->
          string_tbl
        | None ->
          Hashtbl.create 10
      in
      Hashtbl.add string_tbl name i)
    static_table;
  Hashtbl.fold
    (fun length names ret ->
      let bindings = Hashtbl.fold (fun k v lst -> (k, v) :: lst) names [] in
      (length, find_pos names, bindings) :: ret)
    tbl
    []

let mk_static_table static_table =
  let items =
    Array.fold_left
      (fun acc (_, name, value) ->
        let tup =
          Exp.tuple
            [ Exp.constant (Pconst_string (name, ghloc, None))
            ; Exp.constant (Pconst_string (value, ghloc, None))
            ]
        in
        tup :: acc)
      []
      static_table
  in
  let_ "table" (Exp.array (List.rev items))

let mk_lookup_token token_map =
  let_
    "lookup_token"
    (Exp.fun_
       Nolabel
       None
       (Pat.var { txt = "name"; loc = !default_loc })
       (Exp.match_
          (Exp.apply
             (Exp.ident
                { txt = Longident.(Ldot (Lident "String", "length"))
                ; loc = !default_loc
                })
             [ ( Nolabel
               , Exp.ident { txt = Longident.Lident "name"; loc = !default_loc }
               )
             ])
          (List.concat
             [ List.map
                 (fun (length, pos, names) ->
                   Exp.case
                     (Pat.constant
                        (Pconst_integer (string_of_int length, None)))
                     (Exp.match_
                        (Exp.apply
                           (Exp.ident
                              { txt = Ldot (Lident "String", "get")
                              ; loc = !default_loc
                              })
                           [ ( Nolabel
                             , Exp.ident
                                 { txt = Lident "name"; loc = !default_loc } )
                           ; ( Nolabel
                             , Exp.constant
                                 (Pconst_integer (string_of_int pos, None)) )
                           ])
                        (List.concat
                           [ List.map
                               (fun (name, i) ->
                                 Exp.case
                                   (Pat.constant (Pconst_char name.[pos]))
                                   ~guard:
                                     (Exp.apply
                                        (Exp.ident
                                           { txt = Lident "="
                                           ; loc = !default_loc
                                           })
                                        [ ( Nolabel
                                          , Exp.ident
                                              { txt = Lident "name"
                                              ; loc = !default_loc
                                              } )
                                        ; ( Nolabel
                                          , Exp.constant
                                              (Pconst_string (name, ghloc, None))
                                          )
                                        ])
                                   (Exp.constant
                                      (Pconst_integer (string_of_int i, None))))
                               names
                           ; [ Exp.case
                                 (Pat.any ())
                                 (Exp.constant (Pconst_integer ("-1", None)))
                             ]
                           ])))
                 token_map
             ; [ Exp.case
                   (Pat.any ())
                   (Exp.constant (Pconst_integer ("-1", None)))
               ]
             ])))

let () =
  let ic = open_in Sys.argv.(1) in
  let static_table =
    Array.init 61 @@ fun i ->
    let line = input_line ic in
    match String.split_on_char '\t' line with
    | [ s; name ] when int_of_string s == i + 1 ->
      i, name, ""
    | [ s; name; value ] when int_of_string s == i + 1 ->
      i, name, value
    | _ ->
      assert false
  in
  let token_map = make_token_map static_table in
  let ppf = Format.std_formatter in
  Format.fprintf ppf "(* generated by util/gen_static.ml *)\n\n";
  let size = let_ "size" (Exp.constant (Pconst_integer ("61", None))) in
  Pprintast.structure
    ppf
    (List.concat
       [ size :: mk_tokens static_table
       ; [ mk_static_table static_table ]
       ; [ mk_lookup_token token_map ]
       ])
