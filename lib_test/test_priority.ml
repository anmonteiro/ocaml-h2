open H2__
open Test_common
module Scheduler = Server_connection.Scheduler

let pp_priority fmt { Priority.weight; stream_dependency; exclusive } =
  Format.fprintf
    fmt
    "Weight: %d; Parent: %ld; Exclusive: %B"
    weight
    stream_dependency
    exclusive

let priority = Alcotest.of_pp pp_priority

let node =
  (module struct
    open Scheduler.PriorityTreeNode

    type t = parent

    let pp formatter (Parent t) =
      Format.pp_print_text formatter (Scheduler.stream_id t |> Int32.to_string)

    let equal (Parent h1) (Parent h2) =
      Stream_identifier.(Scheduler.(stream_id h1) === Scheduler.(stream_id h2))
  end : Alcotest.TESTABLE
    with type t = Scheduler.PriorityTreeNode.parent)

let default_error_handler ?request:_ _err _handle = ()

let new_p w = { Priority.exclusive = false; stream_dependency = 0l; weight = w }

let test_reqd stream_id =
  Stream.create
    ~max_frame_size:Config.default.read_buffer_size
    stream_id
    Serialize.Writer.(create 0x400)
    default_error_handler
    (fun ~active:_ _ -> ())

let repeat (Scheduler.PriorityTreeNode.Connection root) queue num =
  let rec loop q n acc =
    if n = 0 then
      acc
    else
      match Scheduler.PriorityQueue.pop q with
      | None ->
        failwith "invalid queue"
      | Some ((k, (Scheduler.Stream p as p_node)), q') ->
        (* simulate writing 100 bytes *)
        root.t_last <- p.t;
        Scheduler.update_t p_node 100;
        loop (Scheduler.pq_add k p_node q') (n - 1) (k :: acc)
  in
  loop queue num []

let add_stream root ?(priority = Priority.default_priority) reqd =
  ignore
  @@ Scheduler.add
       root
       ~priority
       ~initial_recv_window_size:Settings.WindowSize.default_initial_window_size
       ~initial_send_window_size:Settings.WindowSize.default_initial_window_size
       reqd

let test_priority_queue () =
  let root = Scheduler.make_root ~capacity:1000 () in
  add_stream root ~priority:(new_p 201) (test_reqd 1l);
  add_stream root ~priority:(new_p 101) (test_reqd 3l);
  add_stream root ~priority:(new_p 1) (test_reqd 5l);
  let q = Scheduler.children root in
  Alcotest.(check bool)
    "Check if empty"
    false
    (Scheduler.PriorityQueue.is_empty q);
  let t = repeat root q 1000 in
  let count_1 = List.filter (fun x -> x = 1l) t |> List.length in
  let count_3 = List.filter (fun x -> x = 3l) t |> List.length in
  let count_5 = List.filter (fun x -> x = 5l) t |> List.length in
  (* After multiple repetitions, the frequency of 1, 3 and 5 is proportional to
   * their weight, e.g. 101 * 1000 / 303 *)
  Alcotest.(check int) "Number of items with weight 201" 663 count_1;
  Alcotest.(check int) "Number of items with weight 101" 333 count_3;
  Alcotest.(check int) "Number of items with weight 1" 4 count_5

let test_reprioritize () =
  let open Scheduler in
  let root = Scheduler.make_root ~capacity:5 () in
  add_stream root (test_reqd 1l);
  add_stream root (test_reqd 3l);
  add_stream root (test_reqd 5l);
  (* change the weight of stream 1 *)
  let new_priority = { Priority.default_priority with weight = 100 } in
  let (Stream stream1 as stream1_node) =
    Scheduler.get_node root 1l |> opt_exn
  in
  Scheduler.reprioritize_stream root ~priority:new_priority stream1_node;
  Alcotest.check
    priority
    "Stream 1 changed weight"
    new_priority
    stream1.priority;
  let (Stream stream3) = Scheduler.get_node root 3l |> opt_exn in
  Alcotest.check
    priority
    "Stream 3 still has default weight"
    Priority.default_priority
    stream3.priority;
  (* Add stream 7 that depends on 1 *)
  let stream7 = test_reqd 7l in
  let stream7_priority =
    { Priority.default_priority with stream_dependency = 1l }
  in
  add_stream root ~priority:stream7_priority stream7;
  let (Stream stream7) = Scheduler.get_node root 7l |> opt_exn in
  Alcotest.check
    priority
    "Stream 7 depends on stream 1"
    stream7_priority
    stream7.priority;
  Alcotest.check
    node
    "Stream 7 depends on stream 1"
    (Parent stream1_node)
    stream7.parent;
  let _, Stream stream1_first_child =
    stream1.children |> PriorityQueue.to_list |> List.hd
  in
  Alcotest.(check bool)
    "Stream 1 has stream 7 in its children"
    false
    (PriorityQueue.is_empty stream1.children);
  Alcotest.(check int32)
    "Stream 1 has stream 7 in its children"
    7l
    stream1_first_child.descriptor.id;
  Alcotest.(check int)
    "Root still has 3 children"
    3
    (PriorityQueue.size (Scheduler.children root))

let test_reprioritize_exclusive () =
  let open Scheduler in
  let root = Scheduler.make_root ~capacity:5 () in
  add_stream root (test_reqd 1l);
  add_stream root (test_reqd 3l);
  add_stream root (test_reqd 5l);
  (* Add stream 7 that exclusively depends on 0 *)
  let stream7 = test_reqd 7l in
  let stream7_priority =
    { Priority.default_priority with stream_dependency = 0l; exclusive = true }
  in
  add_stream root ~priority:stream7_priority stream7;
  let (Stream stream7 as stream7_node) =
    Scheduler.get_node root 7l |> opt_exn
  in
  Alcotest.check
    priority
    "Stream 7 depends on stream 0"
    stream7_priority
    stream7.priority;
  Alcotest.check
    node
    "Stream 7 depends on stream 0"
    (Parent root)
    stream7.parent;
  let root_children = Scheduler.children root |> PriorityQueue.to_list in
  let _, Stream root_first_child = List.hd root_children in
  Alcotest.(check int32)
    "Stream 0 has a single child, stream 7"
    7l
    root_first_child.descriptor.id;
  Alcotest.(check int)
    "Stream 0 has a single child, stream 7"
    1
    (List.length root_children);
  let (Stream stream1) = Scheduler.get_node root 1l |> opt_exn in
  Alcotest.check
    node
    "Stream 1's parent is now stream 7"
    (Parent stream7_node)
    stream1.parent;
  Alcotest.(check int)
    "Stream 7 has 3 children"
    3
    (PriorityQueue.size stream7.children)

let depend_on stream_id =
  { Priority.default_priority with stream_dependency = stream_id }

let set_up_dep_tree root =
  add_stream root (test_reqd 1l);
  add_stream root ~priority:(depend_on 1l) (test_reqd 3l);
  add_stream root ~priority:(depend_on 1l) (test_reqd 5l);
  add_stream root ~priority:(depend_on 5l) (test_reqd 7l);
  add_stream root ~priority:(depend_on 5l) (test_reqd 9l);
  add_stream root ~priority:(depend_on 7l) (test_reqd 11l)

(*
 * This is the tree from: https://tools.ietf.org/html/rfc7540#section-5.3.3
 *
 *          1 --> 7
 *    0                0
 *    |                |
 *    1                7
 *   / \              / \
 *  3   5     ==>    11  1
 *     / \              / \
 *    7   9            3   5
 *    |                    |
 *    11                   9
 *
 *                (non-exclusive)
 *)
let test_reprioritize_to_dependency () =
  let open Scheduler in
  let root = Scheduler.make_root ~capacity:6 () in
  set_up_dep_tree root;
  let (Stream stream1 as stream1_node) =
    Scheduler.get_node root 1l |> opt_exn
  in
  let stream5_node = Scheduler.get_node root 5l |> opt_exn in
  let (Stream stream7 as stream7_node) =
    Scheduler.get_node root 7l |> opt_exn
  in
  Alcotest.check
    node
    "Stream 7 depends on stream 5"
    (Parent stream5_node)
    stream7.parent;
  let root_children = Scheduler.children root |> PriorityQueue.to_list in
  let _, Stream root_first_child = List.hd root_children in
  Alcotest.(check int32)
    "Stream 0 has a single child, stream 1"
    1l
    root_first_child.descriptor.id;
  Alcotest.(check int)
    "Stream 0 has a single child, stream 7"
    1
    (List.length root_children);
  (* reprioritize stream 1 to have 7 as the new parent *)
  reprioritize_stream root ~priority:(depend_on 7l) stream1_node;
  Alcotest.check
    node
    "Stream 1's parent is now stream 7"
    (Parent stream7_node)
    stream1.parent;
  Alcotest.check
    node
    "Stream 7's parent is now stream 0"
    (Parent root)
    stream7.parent;
  Alcotest.(check int)
    "Stream 7 has 2 children"
    2
    (PriorityQueue.size stream7.children);
  Alcotest.(check (list int32))
    "Stream 7 has 2 children, 11 and 1"
    [ 1l; 11l ]
    (stream7.children |> PriorityQueue.to_list |> List.map fst)

(*
 * This is the tree from: https://tools.ietf.org/html/rfc7540#section-5.3.3
 *
 *          1 --> 7
 *    0                0
 *    |                |
 *    1                7
 *   / \               |
 *  3   5     ==>      1
 *     / \            /|\
 *    7   9          3 5 11
 *    |                |
 *    11               9
 *
 *                (exclusive)
 *)
let test_reprioritize_to_dependency_exclusive () =
  let open Scheduler in
  let root = Scheduler.make_root ~capacity:6 () in
  set_up_dep_tree root;
  let stream5_node = Scheduler.get_node root 5l |> opt_exn in
  let (Stream stream7 as stream7_node) =
    Scheduler.get_node root 7l |> opt_exn
  in
  Alcotest.check
    node
    "Stream 7 depends on stream 5"
    (Parent stream5_node)
    stream7.parent;
  let root_children = Scheduler.children root |> PriorityQueue.to_list in
  let _, Stream root_first_child = List.hd root_children in
  Alcotest.(check int32)
    "Stream 0 has a single child, stream 1"
    1l
    root_first_child.descriptor.id;
  Alcotest.(check int)
    "Stream 0 has a single child, stream 7"
    1
    (List.length root_children);
  (* reprioritize stream 1 to have 7 as the new parent with exclusive
     priority *)
  let (Stream stream1 as stream1_node) =
    Scheduler.get_node root 1l |> opt_exn
  in
  reprioritize_stream
    root
    ~priority:
      { Priority.default_priority with
        stream_dependency = 7l
      ; exclusive = true
      }
    stream1_node;
  Alcotest.check
    node
    "Stream 1's parent is now stream 7"
    (Parent stream7_node)
    stream1.parent;
  Alcotest.check
    node
    "Stream 7's parent is now stream 0"
    (Parent root)
    stream7.parent;
  Alcotest.(check int)
    "Stream 7 has a single child"
    1
    (PriorityQueue.size stream7.children);
  Alcotest.(check (list int32))
    "Stream 7 has a single child 1"
    [ 1l ]
    (stream7.children |> PriorityQueue.to_list |> List.map fst);
  Alcotest.(check (list int32))
    "Stream 11 is now a child of stream 1"
    [ 3l; 5l; 11l ]
    (stream1.children |> PriorityQueue.to_list |> List.map fst)

let priority_queue_tests =
  [ "Priority queue tests", `Quick, test_priority_queue ]

let reprioritization_tests =
  [ "Reprioritize simple", `Quick, test_reprioritize
  ; "Reprioritize simple exclusive", `Quick, test_reprioritize_exclusive
  ; "Reprioritize to dependency", `Quick, test_reprioritize_to_dependency
  ; ( "Reprioritize to dependency exclusive"
    , `Quick
    , test_reprioritize_to_dependency_exclusive )
  ]

let () =
  Alcotest.run
    "httpaf unit tests"
    [ "Reprioritization tests", reprioritization_tests
    ; "Priority_Queue_Tests", priority_queue_tests
    ]
