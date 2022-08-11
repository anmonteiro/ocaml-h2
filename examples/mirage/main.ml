(* Generated by _build/default/config.exe configure -t unix (2022-07-31 20:07:52-00:00). *)

open Lwt.Infix
let return = Lwt.return
let run t = OS.Main.run t ; exit
0

let _ = Printexc.record_backtrace true

module Unikernel1 = Unikernel.Make(Tcpip_stack_socket.V4)(Console_unix)
  (Pclock)

module Mirage_logs1 = Mirage_logs.Make(Pclock)

let tcpv4_socket11 = lazy (
  Tcpv4_socket.connect (Key_gen.ipv4 ())
  )

let udpv4_socket11 = lazy (
  Udpv4_socket.connect (Key_gen.ipv4 ())
  )

let argv_unix1 = lazy (
  Bootvar.argv ()
  )

let stackv4_socket1 = lazy (
  let __udpv4_socket11 = Lazy.force udpv4_socket11 in
  let __tcpv4_socket11 = Lazy.force tcpv4_socket11 in
  __udpv4_socket11 >>= fun _udpv4_socket11 ->
  __tcpv4_socket11 >>= fun _tcpv4_socket11 ->
  Tcpip_stack_socket.V4.connect _udpv4_socket11 _tcpv4_socket11
  )

let console_unix_01 = lazy (
  Console_unix.connect "0"
  )

let pclock1 = lazy (
  return ()
  )

let ocaml_gc_control1 = lazy (
  Lwt.return (
let open Gc in
  let ctrl = get () in
  set ({ ctrl with allocation_policy = (match (Key_gen.allocation_policy ()) with `Next_fit -> 0 | `First_fit -> 1 | `Best_fit -> 2);
  minor_heap_size = (match (Key_gen.minor_heap_size ()) with None -> ctrl.minor_heap_size | Some x -> x);
  major_heap_increment = (match (Key_gen.major_heap_increment ()) with None -> ctrl.major_heap_increment | Some x -> x);
  space_overhead = (match (Key_gen.space_overhead ()) with None -> ctrl.space_overhead | Some x -> x);
  max_overhead = (match (Key_gen.max_space_overhead ()) with None -> ctrl.max_overhead | Some x -> x);
  verbose = (match (Key_gen.gc_verbosity ()) with None -> ctrl.verbose | Some x -> x);
  window_size = (match (Key_gen.gc_window_size ()) with None -> ctrl.window_size | Some x -> x);
  custom_major_ratio = (match (Key_gen.custom_major_ratio ()) with None -> ctrl.custom_major_ratio | Some x -> x);
  custom_minor_ratio = (match (Key_gen.custom_minor_ratio ()) with None -> ctrl.custom_minor_ratio | Some x -> x);
  custom_minor_max_size = (match (Key_gen.custom_minor_max_size ()) with None -> ctrl.custom_minor_max_size | Some x -> x) })
)
  )

let ocaml_hashtable_randomize1 = lazy (
  Lwt.return (if (Key_gen.randomize_hashtables ()) then Hashtbl.randomize ())
  )

let ocaml_backtrace1 = lazy (
  Lwt.return (Printexc.record_backtrace (Key_gen.backtrace ()))
  )

let key1 = lazy (
  let __argv_unix1 = Lazy.force argv_unix1 in
  __argv_unix1 >>= fun _argv_unix1 ->
  return (Functoria_runtime.with_argv (List.map fst Key_gen.runtime_keys) "h2_unikernel" _argv_unix1)
  )

let f11 = lazy (
  let __stackv4_socket1 = Lazy.force stackv4_socket1 in
  let __console_unix_01 = Lazy.force console_unix_01 in
  let __pclock1 = Lazy.force pclock1 in
  __stackv4_socket1 >>= fun _stackv4_socket1 ->
  __console_unix_01 >>= fun _console_unix_01 ->
  __pclock1 >>= fun _pclock1 ->
  Unikernel1.start _stackv4_socket1 _console_unix_01 _pclock1
  )

let mirage_logs1 = lazy (
  let __pclock1 = Lazy.force pclock1 in
  __pclock1 >>= fun _pclock1 ->
  let ring_size = None in
  let reporter = Mirage_logs1.create ?ring_size () in
  Mirage_runtime.set_level ~default:Logs.Info (Key_gen.logs ());
  Mirage_logs1.set_reporter reporter;
  Lwt.return reporter
  )

let mirage1 = lazy (
  let __key1 = Lazy.force key1 in
  let __ocaml_backtrace1 = Lazy.force ocaml_backtrace1 in
  let __ocaml_hashtable_randomize1 = Lazy.force ocaml_hashtable_randomize1 in
  let __ocaml_gc_control1 = Lazy.force ocaml_gc_control1 in
  let __mirage_logs1 = Lazy.force mirage_logs1 in
  let __f11 = Lazy.force f11 in
  __key1 >>= fun _key1 ->
  __ocaml_backtrace1 >>= fun _ocaml_backtrace1 ->
  __ocaml_hashtable_randomize1 >>= fun _ocaml_hashtable_randomize1 ->
  __ocaml_gc_control1 >>= fun _ocaml_gc_control1 ->
  __mirage_logs1 >>= fun _mirage_logs1 ->
  __f11 >>= fun _f11 ->
  Lwt.return_unit
  )

let () =
  let t =
  Lazy.force key1 >>= fun _ ->
    Lazy.force ocaml_backtrace1 >>= fun _ ->
    Lazy.force ocaml_hashtable_randomize1 >>= fun _ ->
    Lazy.force ocaml_gc_control1 >>= fun _ ->
    Lazy.force mirage_logs1 >>= fun _ ->
    Lazy.force mirage1
  in run t
