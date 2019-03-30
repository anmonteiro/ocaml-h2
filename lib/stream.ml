module AB = Angstrom.Buffered

type partial_headers =
  { mutable parse_state : (Headers.t, Hpack.error) result AB.state
  ; end_stream          : bool
  }

type closed_reason =
  | Finished
  (* TODO: we could abide by the following by either 1) having I/O runtime
     support for timers or 2) by simply counting the number of frames received
     after we've sent an RST_STREAM?

     From RFC7540§5.4.2:
       Normally, an endpoint SHOULD NOT send more than one RST_STREAM frame for
       any stream. However, an endpoint MAY send additional RST_STREAM frames
       if it receives frames on a closed stream after more than a round-trip
       time. This behavior is permitted to deal with misbehaving
       implementations. *)
  | ResetByUs of Error.error_code
    (* Received an RST_STREAM frame from the peer. *)
  | ResetByThem of Error.error_code

type closed =
  { reason : closed_reason
    (* When a stream is closed, we may want to keep it around in the hash
     * table for a while (e.g. to know whether this stream was reset by the
     * peer - some error handling code depends on that). We start with a
     * default value, and on every writer yield we decrement it. If it
     * reaches 0, the stream is finally removed from the hash table. *)
  ; mutable ttl : int
  }
