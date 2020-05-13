(*----------------------------------------------------------------------------
 *  Copyright (c) 2019 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

type t =
  (* From RFC7540§7:
   *   NO_ERROR (0x0): The associated condition is not a result of an
   *   error. *)
  | NoError
  (* From RFC7540§7:
   *   PROTOCOL_ERROR (0x1): The endpoint detected an unspecific protocol
   *   error. This error is for use when a more specific error code is not
   *   available. *)
  | ProtocolError
  (* From RFC7540§7:
   *   INTERNAL_ERROR (0x2): The endpoint encountered an unexpected internal
   *   error. *)
  | InternalError
  (* From RFC7540§7:
   *   FLOW_CONTROL_ERROR (0x3): The endpoint detected that its peer violated
   *   the flow-control protocol. *)
  | FlowControlError
  (* From RFC7540§7:
   *   SETTINGS_TIMEOUT (0x4): The endpoint sent a SETTINGS frame but did not
   *   receive a response in a timely manner. *)
  | SettingsTimeout
  (* From RFC7540§7:
   *   STREAM_CLOSED (0x5): The endpoint received a frame after a stream was
   *   half-closed. *)
  | StreamClosed
  (* From RFC7540§7:
   *   FRAME_SIZE_ERROR (0x6): The endpoint received a frame with an invalid
   *   size. *)
  | FrameSizeError
  (* From RFC7540§7:
   *   REFUSED_STREAM (0x7): The endpoint refused the stream prior to
   *   performing any application processing (see Section 8.1.4 for
   *   details). *)
  | RefusedStream
  (* From RFC7540§7:
   *   CANCEL (0x8): Used by the endpoint to indicate that the stream is no
   *   longer needed. *)
  | Cancel
  (* From RFC7540§7:
   *   COMPRESSION_ERROR (0x9): The endpoint is unable to maintain the header
   *   compression context for the connection. *)
  | CompressionError
  (* From RFC7540§7:
   *   CONNECT_ERROR (0xa): The connection established in response to a
   *   CONNECT request (Section 8.3) was reset or abnormally closed. *)
  | ConnectError
  (* From RFC7540§7:
   *   ENHANCE_YOUR_CALM (0xb): The endpoint detected that its peer is
   *   exhibiting a behavior that might be generating excessive load. *)
  | EnhanceYourCalm
  (* From RFC7540§7:
   *   INADEQUATE_SECURITY (0xc): The underlying transport has properties
   *   that do not meet minimum security requirements (see Section 9.2). *)
  | InadequateSecurity
  (* From RFC7540§7:
   *   HTTP_1_1_REQUIRED (0xd): The endpoint requires that HTTP/1.1 be used
   *   instead of HTTP/2. *)
  | HTTP_1_1_Required
  (* From RFC7540§7:
   *   Unknown or unsupported error codes MUST NOT trigger any special
   *   behavior. These MAY be treated by an implementation as being
   *   equivalent to INTERNAL_ERROR. *)
  | UnknownError_code of int32

(* From RFC7540§7:
 *   Error codes are 32-bit fields that are used in RST_STREAM and GOAWAY
 *   frames to convey the reasons for the stream or connection error. *)
let serialize = function
  | NoError ->
    0x0l
  | ProtocolError ->
    0x1l
  | InternalError ->
    0x2l
  | FlowControlError ->
    0x3l
  | SettingsTimeout ->
    0x4l
  | StreamClosed ->
    0x5l
  | FrameSizeError ->
    0x6l
  | RefusedStream ->
    0x7l
  | Cancel ->
    0x8l
  | CompressionError ->
    0x9l
  | ConnectError ->
    0xal
  | EnhanceYourCalm ->
    0xbl
  | InadequateSecurity ->
    0xcl
  | HTTP_1_1_Required ->
    0xdl
  | UnknownError_code id ->
    id

let parse = function
  | 0x0l ->
    NoError
  | 0x1l ->
    ProtocolError
  | 0x2l ->
    InternalError
  | 0x3l ->
    FlowControlError
  | 0x4l ->
    SettingsTimeout
  | 0x5l ->
    StreamClosed
  | 0x6l ->
    FrameSizeError
  | 0x7l ->
    RefusedStream
  | 0x8l ->
    Cancel
  | 0x9l ->
    CompressionError
  | 0xal ->
    ConnectError
  | 0xbl ->
    EnhanceYourCalm
  | 0xcl ->
    InadequateSecurity
  | 0xdl ->
    HTTP_1_1_Required
  | id ->
    UnknownError_code id

let to_string = function
  | NoError ->
    "NO_ERROR (0x0)"
  | ProtocolError ->
    "PROTOCOL_ERROR (0x1)"
  | InternalError ->
    "INTERNAL_ERROR (0x2)"
  | FlowControlError ->
    "FLOW_CONTROL_ERROR (0x3)"
  | SettingsTimeout ->
    "SETTINGS_TIMEOUT (0x4)"
  | StreamClosed ->
    "STREAM_CLOSED (0x5)"
  | FrameSizeError ->
    "FRAME_SIZE_ERROR (0x6)"
  | RefusedStream ->
    "REFUSED_STREAM (0x7)"
  | Cancel ->
    "CANCEL (0x8)"
  | CompressionError ->
    "COMPRESSION_ERROR (0x9)"
  | ConnectError ->
    "CONNECT_ERROR (0xa)"
  | EnhanceYourCalm ->
    "ENHANCE_YOUR_CALM (0xb)"
  | InadequateSecurity ->
    "INADEQUATE_SECURITY (0xc)"
  | HTTP_1_1_Required ->
    "HTTP_1_1_REQUIRED (0xd)"
  | UnknownError_code id ->
    Format.asprintf "UNKNOWN_ERROR (0x%lx)" id

let pp_hum formatter t = Format.fprintf formatter "%s" (to_string t)
