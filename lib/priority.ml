open Util

type t =
  { exclusive : bool
  ; stream_dependency : Stream_identifier.t
  ; weight : int
  }

(* From RFC7540§5.3.5:
     All streams are initially assigned a non-exclusive dependency on stream
     0x0. Pushed streams (Section 8.2) initially depend on their associated
     stream. In both cases, streams are assigned a default weight of 16. *)
let default_priority =
  { exclusive = false
  ; stream_dependency = 0l
  ; weight = 16
  }


(* From RFC7540§5.4.1:
     All dependent streams are allocated an integer weight between 1 and 256
     (inclusive). *)
let highest_priority =
  { exclusive = false
  ; stream_dependency = 0l
  ; weight = 256
  }

(* --- Exclusive flag ---

  From RFC7540§5.4.1:
    +-+-------------------------------------------------------------+
    |E|                  Stream Dependency (31)                     |
    +-+-------------+-----------------------------------------------+
    |   Weight (8)  |
    +-+-------------+
*)

let test_exclusive n = test_bit_int32 n 31

let set_exclusive n = set_bit_int32 n 31

let clear_exclusive n = clear_bit_int32 n 31

let equal p1 p2 =
  p1.weight == p2.weight &&
  Int32.equal p1.stream_dependency p2.stream_dependency &&
  p1.exclusive == p2.exclusive
