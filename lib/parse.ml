(*----------------------------------------------------------------------------
    Copyright (c) 2017 Inhabited Type LLC.
    Copyright (c) 2019 Antonio N. Monteiro.

    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in the
       documentation and/or other materials provided with the distribution.

    3. Neither the name of the author nor the names of his contributors
       may be used to endorse or promote products derived from this software
       without specific prior written permission.

    THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
    OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
    WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
    DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
    ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
    DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
    OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
    HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
    STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
  ----------------------------------------------------------------------------*)

open Angstrom

type parse_context =
  { mutable frame_header : Frame.frame_header option
  ; mutable remaining_bytes_to_skip : int
  ; mutable did_report_stream_error : bool
  }

let error e =
  Error e

let connection_error error_code msg =
  error Error.(ConnectionError (error_code, msg))

let stream_error error_code stream_id =
  error Error.(StreamError (stream_id, error_code))

let parse_uint24 o1 o2 o3 =
  (o1 lsl 16) lor (o2 lsl 8) lor o3

let frame_length =
  (* From RFC7540§4.1:
   *   Length: The length of the frame payload expressed as an unsigned 24-bit
   *   integer. *)
  lift3 parse_uint24 any_uint8 any_uint8 any_uint8

let frame_type =
  (* From RFC7540§4.1:
   *   Type: The 8-bit type of the frame. The frame type determines the format
   *   and semantics of the frame. Implementations MUST ignore and discard any
   *   frame that has a type that is unknown. *)
  lift Frame.FrameType.parse any_uint8

let flags =
  (* From RFC7540§4.1:
   *   Flags: An 8-bit field reserved for boolean flags specific to the frame
   *   type. *)
  any_uint8

let parse_stream_identifier n =
  (* From RFC7540§4.1:
   *   Stream Identifier: A stream identifier (see Section 5.1.1) expressed as
   *   an unsigned 31-bit integer. The value 0x0 is reserved for frames that
   *   are associated with the connection as a whole as opposed to an
   *   individual stream. *)
  Int32.(logand n (sub (shift_left 1l 31) 1l))

let stream_identifier = lift parse_stream_identifier BE.any_int32

let parse_frame_header =
  lift4
    (fun payload_length frame_type flags stream_id ->
      { Frame.flags; payload_length; stream_id; frame_type })
    frame_length frame_type flags stream_identifier
  <?> "frame_header"
  (* TODO: explain why we commit here. *)
  <* commit

let parse_padded_payload { Frame.payload_length; flags; _ } parser =
  if Flags.test_padded flags then begin
    any_uint8 >>= fun pad_length ->
      (* From RFC7540§6.1:
       *   Pad Length: An 8-bit field containing the length of the frame
       *   padding in units of octets.
       *
       *   Data: Application data. The amount of data is the remainder of the
       *   frame payload after subtracting the length of the other fields that
       *   are present.
       *
       *   Padding: Padding octets that contain no application semantic
       *   value. *)
      if pad_length >= payload_length then
        (* From RFC7540§6.1:
         *   If the length of the padding is the length of the frame payload or
         *   greater, the recipient MUST treat this as a connection error
         *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
        advance (payload_length - 1) >>| fun () ->
          connection_error ProtocolError "Padding size exceeds payload size"
      else begin
        (* Subtract the octet that contains the length of padding, and the
         * padding octets. *)
        let relevant_length = payload_length - 1 - pad_length in
        parser relevant_length <* advance pad_length
      end
  end else
    parser payload_length

let parse_data_frame ({ Frame.stream_id; payload_length; _ } as frame_header) =
  if Stream_identifier.is_connection stream_id then
    (* From RFC7540§6.1:
     *   DATA frames MUST be associated with a stream. If a DATA frame is
     *   received whose stream identifier field is 0x0, the recipient MUST
     *   respond with a connection error (Section 5.4.1) of type
     *   PROTOCOL_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error
        ProtocolError "Data frames must be associated with a stream"
  else
    let parse_data length =
      lift (fun bs -> Ok (Frame.Data bs)) (take_bigstring length)
    in
    parse_padded_payload frame_header parse_data

let parse_priority =
  lift2 (fun stream_dependency weight ->
    let e = Priority.test_exclusive stream_dependency in
    { Priority
    . exclusive = e
      (* From RFC7540§6.3:
       *   An unsigned 8-bit integer representing a priority weight for the
       *   stream (see Section 5.3). Add one to the value to obtain a weight
       *   between 1 and 256. *)
    ; weight = weight + 1
    ; stream_dependency = parse_stream_identifier stream_dependency
    })
    BE.any_int32 any_uint8

let parse_headers_frame frame_header =
  let { Frame.payload_length; stream_id; flags; _ } = frame_header in
  if Stream_identifier.is_connection stream_id then
    (* From RFC7540§6.2:
     *   HEADERS frames MUST be associated with a stream. If a HEADERS frame is
     *   received whose stream identifier field is 0x0, the recipient MUST
     *   respond with a connection error (Section 5.4.1) of type
     *   PROTOCOL_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error ProtocolError "HEADERS must be associated with a stream"
  else
    let parse_headers length =
      if Flags.test_priority flags then
        lift2
          (fun priority headers ->
            Ok (Frame.Headers (Some priority, headers)))
          parse_priority
          (* See RFC7540§6.3:
           *   Stream Dependency (4 octets) + Weight (1 octet). *)
          (take_bigstring (length - 5))
      else
        lift (fun headers_block ->
          Ok (Frame.Headers (None, headers_block)))
          (take_bigstring length)
    in
    parse_padded_payload frame_header parse_headers

let parse_priority_frame { Frame.payload_length; stream_id; _ } =
  if Stream_identifier.is_connection stream_id then
    (* From RFC7540§6.3:
     *   The PRIORITY frame always identifies a stream. If a PRIORITY frame is
     *   received with a stream identifier of 0x0, the recipient MUST respond
     *   with a connection error (Section 5.4.1) of type PROTOCOL_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error
        ProtocolError "PRIORITY must be associated with a stream"
  else if payload_length <> 5 then
    (* From RFC7540§6.3:
     *   A PRIORITY frame with a length other than 5 octets MUST be treated as
     *   a stream error (Section 5.4.2) of type FRAME_SIZE_ERROR. *)
    advance payload_length >>| fun () ->
      stream_error FrameSizeError stream_id
  else
    lift (fun priority -> Ok (Frame.Priority priority)) parse_priority

let parse_error_code =
  lift Error.parse BE.any_int32

let parse_rst_stream_frame { Frame.payload_length; stream_id; _ } =
  if Stream_identifier.is_connection stream_id then
    (* From RFC7540§6.4:
     *   RST_STREAM frames MUST be associated with a stream. If a RST_STREAM
     *   frame is received with a stream identifier of 0x0, the recipient MUST
     *   treat this as a connection error (Section 5.4.1) of type
     *   PROTOCOL_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error
        ProtocolError "RST_STREAM must be associated with a stream"
  else if payload_length != 4 then
    (* From RFC7540§6.4:
     *   A RST_STREAM frame with a length other than 4 octets MUST be treated
     *   as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error
        FrameSizeError "RST_STREAM payload must be 4 octets in length"
  else
    lift (fun error_code -> Ok (Frame.RSTStream error_code)) parse_error_code

let parse_settings_frame { Frame.payload_length; stream_id; flags; _ } =
  if not (Stream_identifier.is_connection stream_id) then
    (* From RFC7540§6.5:
     *   If an endpoint receives a SETTINGS frame whose stream identifier field
     *   is anything other than 0x0, the endpoint MUST respond with a
     *   connection error (Section 5.4.1) of type PROTOCOL_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error
        ProtocolError "SETTINGS must be associated with stream id 0x0"
  else if payload_length mod 6 != 0 then
    (* From RFC7540§6.5:
     *   A SETTINGS frame with a length other than a multiple of 6 octets MUST
     *   be treated as a connection error (Section 5.4.1) of type
     *   FRAME_SIZE_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error
        FrameSizeError "SETTINGS payload size must be a multiple of 6"
  else if Flags.test_ack flags && payload_length != 0 then
    (* From RFC7540§6.5:
     *   Receipt of a SETTINGS frame with the ACK flag set and a length field
     *   value other than 0 MUST be treated as a connection error
     *   (Section 5.4.1) of type FRAME_SIZE_ERROR. *)
    advance payload_length >>| fun () ->
     connection_error FrameSizeError "SETTINGS with ACK must be empty"
  else begin
    (* From RFC7540§6.5.1:
     *   The payload of a SETTINGS frame consists of zero or more parameters,
     *   each consisting of an unsigned 16-bit setting identifier and an
     *   unsigned 32-bit value. *)
    let num_settings = payload_length / 6 in
    let parse_setting =
      lift2 (fun k v ->
        match Settings.parse_key k with
        | Some s -> Some (s, Int32.to_int v)
        | None -> None)
        BE.any_uint16 BE.any_int32
    in
    (* Note: This ignores unknown settings.
     *
     * From RFC7540§6.5.3:
     *   Unsupported parameters MUST be ignored.
     *)
    lift (fun xs ->
      let rec filter_opt acc = function
        | [] -> acc []
        | Some x :: xs ->
            filter_opt (fun ys -> acc (x :: ys)) xs
        | None :: xs -> filter_opt acc xs
      in
      (* From RFC7540§6.5.3:
       *   The values in the SETTINGS frame MUST be processed in the order they
       *   appear, with no other frame processing between values. *)
      Ok (Frame.Settings (filter_opt (fun x -> x) xs)))
    (count num_settings parse_setting)
  end

let parse_push_promise_frame frame_header =
  let { Frame.payload_length; stream_id; _ } = frame_header in
  if Stream_identifier.is_connection stream_id then
    (* From RFC7540§6.6:
     *   The stream identifier of a PUSH_PROMISE frame indicates the
     *   stream it is associated with. If the stream identifier field
     *   specifies the value 0x0, a recipient MUST respond with a
     *   connection error (Section 5.4.1) of type PROTOCOL_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error ProtocolError "PUSH must be associated with a stream"
  else
    let parse_push_promise length =
      lift2 (fun promised_stream_id fragment ->
        if Stream_identifier.is_connection promised_stream_id then
          (* From RFC7540§6.6:
           *   A receiver MUST treat the receipt of a PUSH_PROMISE that
           *   promises an illegal stream identifier (Section 5.1.1) as a
           *   connection error (Section 5.4.1) of type PROTOCOL_ERROR. *)
          connection_error
            ProtocolError "PUSH must not promise stream id 0x0"
        else if Stream_identifier.is_request promised_stream_id then
          (* From RFC7540§6.6:
           *   A receiver MUST treat the receipt of a PUSH_PROMISE that
           *   promises an illegal stream identifier (Section 5.1.1) as a
           *   connection error (Section 5.4.1) of type PROTOCOL_ERROR.
           *
           * Note: An odd-numbered stream is an invalid stream identifier for
           * the server, and only the server can send PUSH_PROMISE frames. *)
          connection_error
            ProtocolError
            "PUSH must be associated with an even-numbered stream id"
        else
          Ok Frame.(PushPromise (promised_stream_id, fragment)))
      stream_identifier
      (* From RFC7540§6.6:
       *   The PUSH_PROMISE frame includes the unsigned 31-bit identifier of
       *   the stream the endpoint plans to create along with a set of headers
       *   that provide additional context for the stream. *)
      (take_bigstring (length - 4))
    in
    parse_padded_payload frame_header parse_push_promise

let parse_ping_frame { Frame.payload_length; stream_id; _ } =
  if not (Stream_identifier.is_connection stream_id) then
    (* From RFC7540§6.7:
     *   PING frames are not associated with any individual stream. If a PING
     *   frame is received with a stream identifier field value other than
     *   0x0, the recipient MUST respond with a connection error
     *   (Section 5.4.1) of type PROTOCOL_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error
        ProtocolError "PING must be associated with stream id 0x0"
  else if payload_length != 8 then
    (* From RFC7540§6.7:
     *   Receipt of a PING frame with a length field value other than 8 MUST
     *   be treated as a connection error (Section 5.4.1) of type
     *   FRAME_SIZE_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error FrameSizeError "PING payload must be 8 octets in length"
  else
    lift (fun bs -> Ok (Frame.Ping bs))
      (take_bigstring payload_length)

let parse_go_away_frame { Frame.payload_length; stream_id; _ } =
  if not (Stream_identifier.is_connection stream_id) then
    (* From RFC7540§6.8:
     *   The GOAWAY frame applies to the connection, not a specific stream. An
     *   endpoint MUST treat a GOAWAY frame with a stream identifier other than
     *   0x0 as a connection error (Section 5.4.1) of type PROTOCOL_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error
        ProtocolError "GOAWAY must be associated with stream id 0x0"
  else
    lift3
      (fun last_stream_id err debug_data ->
        Ok (Frame.GoAway (last_stream_id, err, debug_data)))
      stream_identifier
      parse_error_code
      (take_bigstring (payload_length - 8))

let parse_window_update_frame { Frame.stream_id; payload_length; _ } =
  (* From RFC7540§6.9:
   *   A WINDOW_UPDATE frame with a length other than 4 octets MUST be treated
   *   as a connection error (Section 5.4.1) of type FRAME_SIZE_ERROR. *)
  if payload_length != 4 then
    advance payload_length >>| fun () ->
      connection_error
        FrameSizeError "WINDOW_UPDATE payload must be 4 octets in length"
  else
  lift (fun uint ->
    let window_size_increment = Util.clear_bit (Int32.to_int uint) 31 in
    if window_size_increment == 0 then begin
      (* From RFC7540§6.9:
       *   A receiver MUST treat the receipt of a WINDOW_UPDATE frame with an
       *   flow-control window increment of 0 as a stream error (Section 5.4.2)
       *   of type PROTOCOL_ERROR; errors on the connection flow-control window
       *   MUST be treated as a connection error (Section 5.4.1). *)
      if Stream_identifier.is_connection stream_id then
        connection_error ProtocolError "Window update must not be 0"
      else
        stream_error ProtocolError stream_id
    end else
      Ok (Frame.WindowUpdate window_size_increment))
  BE.any_int32

let parse_continuation_frame { Frame.payload_length; stream_id; _ } =
  if Stream_identifier.is_connection stream_id then
    (* From RFC7540§6.10:
     *   CONTINUATION frames MUST be associated with a stream. If a
     *   CONTINUATION frame is received whose stream identifier field is 0x0,
     *   the recipient MUST respond with a connection error (Section 5.4.1) of
     *   type PROTOCOL_ERROR. *)
    advance payload_length >>| fun () ->
      connection_error
        ProtocolError "CONTINUATION must be associated with a stream"
  else
    lift (fun block_fragment -> Ok (Frame.Continuation block_fragment))
      (take_bigstring payload_length)

let parse_unknown_frame typ { Frame.payload_length; _ } =
  lift (fun bigstring -> Ok (Frame.Unknown (typ, bigstring)))
    (take_bigstring payload_length)

let parse_frame_payload ({ Frame.frame_type; _ } as frame_header) =
  begin match frame_type with
  | Frame.FrameType.Data -> parse_data_frame frame_header
  | Headers -> parse_headers_frame frame_header
  | Priority -> parse_priority_frame frame_header
  | RSTStream -> parse_rst_stream_frame frame_header
  | Settings -> parse_settings_frame frame_header
  | PushPromise -> parse_push_promise_frame frame_header
  | Ping -> parse_ping_frame frame_header
  | GoAway -> parse_go_away_frame frame_header
  | WindowUpdate -> parse_window_update_frame frame_header
  | Continuation -> parse_continuation_frame frame_header
  | Unknown typ -> parse_unknown_frame typ frame_header
  end
  <?> "frame_payload"

let parse_frame parse_context =
  parse_frame_header >>= fun ({ Frame.payload_length; _ } as frame_header) ->
    (* If we're parsing a new frame, we didn't yet send a stream error on it *)
    parse_context.did_report_stream_error <- false;
    parse_context.frame_header <- Some frame_header;
    (* Payload could be 0 (e.g. empty SETTINGS frame). This always succeeds. *)
    Angstrom.Unsafe.peek 0 (fun bs ~off:_ ~len:_ ->
      (* h2 does unbuffered parsing and the bigarray we read input from is
       * allocated based on the maximum frame payload negotiated by HTTP/2
       * communication. If the underlying buffer is smaller than what
       * the frame can fit, we want to skip the remaining input and skip to the
       * next frame.
       *
       * From RFC7540§5.4.2:
       *   A stream error is an error related to a specific stream that does
       *   not affect processing of other streams. *)
      let is_frame_size_error = payload_length > Bigstringaf.length bs in
      if is_frame_size_error then
        parse_context.remaining_bytes_to_skip <-
          parse_context.remaining_bytes_to_skip + payload_length)
    >>= fun () ->
      lift (function
        | Ok frame_payload ->
          Ok { Frame
             . frame_header
             ; frame_payload
             }
        | Error e -> Error e)
        (parse_frame_payload frame_header)

(* This is the client connection preface. *)
let connection_preface =
  (* From RFC7540§3.5:
   *   In HTTP/2, each endpoint is required to send a connection preface as a
   *   final confirmation of the protocol in use and to establish the initial
   *   settings for the HTTP/2 connection. *)
  string "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n" <?> "connection preface"

module Reader = struct
  module AU = Angstrom.Unbuffered

  type parse_error =
      (* Parse error reported by Angstrom *)
    [ `Parse of string list * string
      (* Full error information *)
    | `Error of Error.t
      (* Just the error code, need to puzzle back connection or stream info *)
    | `ErrorCode of Error.error_code
    ]

  type 'error parse_state =
    | Done
    | Fail    of 'error
    | Partial of (Bigstringaf.t -> off:int -> len:int -> AU.more -> (unit, 'error) result AU.state)

  type 'error t =
    { parser              : (unit, 'error) result Angstrom.t
    ; mutable parse_state : 'error parse_state
      (* The state of the parse for the current request *)
    ; mutable closed      : bool
      (* Whether the input source has left the building, indicating that no
       * further input will be received. *)
    ; parse_context : parse_context
      (* The current stream identifier being processed, in order to discern
       * whether the error that needs to be assembled is a stream or connection
       * error. *)
    }

  type client_connection_preface = parse_error t
  type server_connection_preface = parse_error t
  type frame = parse_error t

  let create parser parse_context =
    { parser
    ; parse_state = Done
    ; closed      = false
    ; parse_context
    }

  let create_parse_context () =
    { frame_header = None
    ; remaining_bytes_to_skip = 0
    ; did_report_stream_error = false
    }

  let settings_preface handler parse_context =
    (* From RFC7540§3.5:
     *   [...] the connection preface starts with the string
     *   PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n). This sequence MUST be followed by
     *   a SETTINGS frame (Section 6.5), which MAY be empty. *)
    parse_frame parse_context >>| function
    | Ok ({ frame_payload = Frame.Settings settings_list; _ } as frame) ->
      handler frame settings_list;
      Ok ()
    | Ok _ ->
      (* From RFC7540§3.5:
       *   Clients and servers MUST treat an invalid connection preface as a
       *   connection error (Section 5.4.1) of type PROTOCOL_ERROR. A GOAWAY
       *   frame (Section 6.8) MAY be omitted in this case, since an invalid
       *   preface indicates that the peer is not using HTTP/2. *)
      Error
        (`Error
          Error.(ConnectionError
            (ProtocolError, "Invalid connection preface")))
    | Error e ->
      Error (`Error e)

  let client_connection_preface handler =
    let parse_context = create_parse_context () in
    let parser =
      connection_preface *> settings_preface handler parse_context
    in
    create parser parse_context

  let server_connection_preface handler =
    let parse_context = create_parse_context () in
    let parser = settings_preface handler parse_context in
    create parser parse_context

  let frame handler =
    let parse_context = create_parse_context () in
    let parser =
      skip_many (parse_frame parse_context <* commit >>| handler)
      >>| fun () -> Ok ()
    in
    create parser parse_context

  let is_closed t =
    t.closed

  let transition t state =
    match state with
    | AU.Done(consumed, Ok ()) ->
      t.parse_state <- Done;
      consumed
    | Done(consumed, Error error) ->
      t.parse_state <- Fail error;
      consumed
    | Fail(consumed, marks, msg) ->
      t.parse_state <- Fail (`Parse(marks, msg));
      consumed
    | Partial { committed; continue } ->
      (* If we have bytes to skip over then it means we've spotted a
       * FRAME_SIZE_ERROR, a case where, due to our unbuffered parsing, the
       * payload length declared in a frame header is larger than the
       * underlying buffer can fit. *)
      if t.parse_context.remaining_bytes_to_skip > 0 then
        t.parse_state <- Fail (`ErrorCode Error.FrameSizeError)
      else
        t.parse_state <- Partial continue;
      committed

  let start t state =
      match state with
      | AU.Done _         -> failwith "h2.Parse.unable to start parser"
      | Fail(0, marks, msg) ->
        t.parse_state <- Fail (`Parse(marks, msg))
      | Partial { committed = 0; continue } ->
        t.parse_state <- Partial continue
      | Partial _
      | Fail _ -> assert false

  let rec read_with_more t bs ~off ~len more =
    let consumed = match t.parse_state with
    | Fail _ ->
      let parser_ctx = t.parse_context in
      let remaining_bytes = parser_ctx.remaining_bytes_to_skip in
      if remaining_bytes > 0 then begin
        (* Just skip input if we need to *)
        assert (remaining_bytes >= len);
        let remaining_bytes' = remaining_bytes - len in
        parser_ctx.remaining_bytes_to_skip <- remaining_bytes';
        assert (remaining_bytes' >= 0);
        if remaining_bytes' = 0 then
          (* Reset the parser state to `Done` so that we can read the next
           * frame (after skipping through the bad input) *)
          t.parse_state <- Done;
        len
      end else
        0
    | Done   ->
      start t (AU.parse t.parser);
      read_with_more t bs ~off ~len more;
    | Partial continue ->
      transition t (continue bs more ~off ~len)
    in
    begin match more with
    | Complete -> t.closed <- true;
    | Incomplete -> ()
    end;
    consumed

  let force_close t =
    ignore (read_with_more t Bigstringaf.empty ~off:0 ~len:0 Complete : int)

  let state_to_string = function
    | Fail _ -> "fail"
    | Done -> "done"
    | Partial _ -> "partial"

  let fail_to_string marks err =
    String.concat " > " marks ^ ": " ^ err

  let next_from_error t ?(msg="") error_code =
    match t.parse_context, error_code with
    | { frame_header = Some { frame_type = Headers | PushPromise | Continuation | Settings; _ }; _ },
      Error.FrameSizeError
    | { frame_header = Some { Frame.stream_id = 0x0l; _ }; _ }, _
    | { frame_header = None; _ }, _ ->
      (* From RFC7540§4.2:
       *   A frame size error in a frame that could alter the state of the
       *   entire connection MUST be treated as a connection error (Section
       *   5.4.1); this includes any frame carrying a header block (Section
       *   4.3) (that is, HEADERS, PUSH_PROMISE, and CONTINUATION), SETTINGS,
       *   and any frame with a stream identifier of 0. *)
      `Error Error.(ConnectionError (error_code, msg))
    | { frame_header = Some _; did_report_stream_error = true; _ }, _ ->
      (* If the parser is in a `Fail` state and would report a stream error,
       * just issue a `Read` operation if we've already reported that error. *)
      if t.closed then
        `Close
      else
        `Read
    | { frame_header = Some { Frame.stream_id; _ }; _ }, _ ->
      t.parse_context.did_report_stream_error <- true;
      `Error Error.(StreamError (stream_id, error_code))

  let next t =
    match t.parse_state with
    | Partial _ -> `Read
    | Done ->
      if t.closed then
        `Close
      else
        `Read
    | Fail (`Error e) -> `Error e
    | Fail (`ErrorCode error_code) ->
      next_from_error t error_code
    | Fail (`Parse (marks, msg)) ->
      let error_code = match marks, msg with
      | ["frame_payload"], "not enough input" ->
        (* From RFC7540§4.2:
         *   An endpoint MUST send an error code of FRAME_SIZE_ERROR if a frame
         *   exceeds the size defined in SETTINGS_MAX_FRAME_SIZE, exceeds any
         *   limit defined for the frame type, or is too small to contain
         *   mandatory frame data. *)
        Error.FrameSizeError
      | _ -> Error.ProtocolError
      in
      next_from_error t ~msg:(fail_to_string marks msg) error_code
end
