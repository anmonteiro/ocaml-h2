(* From RFC7540§5.1.1:
     Streams are identified with an unsigned 31-bit integer. *)
type t = int32

let (===) = Int32.equal

let[@inline] (<=) s1 s2 =
  Int32.compare s1 s2 <= 0

let[@inline] (>) s1 s2 =
  Int32.compare s1 s2 > 0

let[@inline] (>=) s1 s2 =
  Int32.compare s1 s2 >= 0

(* From RFC7540§5.1.1:
     A stream identifier of zero (0x0) is used for connection control messages;
     the stream identifier of zero cannot be used to establish a new stream. *)
let connection = Int32.zero

(* From RFC7540§5.1.1:
     A stream identifier of zero (0x0) is used for connection control messages;
     the stream identifier of zero cannot be used to establish a new stream. *)
let[@inline] is_connection id = Int32.equal id connection

(* From RFC7540§5.1.1:
     Streams initiated by a client MUST use odd-numbered stream
     identifiers [...]. *)
let[@inline] is_request id = (Int32.rem id 2l) === 1l

(* From RFC7540§5.1.1:
     Streams initiated by [...] the server MUST use even-numbered stream
     identifiers. A stream identifier of zero (0x0) is used for connection
     control messages [...]. *)
let[@inline] is_pushed = function
  | 0l -> false
  | n -> Int32.rem n 2l === 0l

