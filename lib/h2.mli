(*----------------------------------------------------------------------------
 *  Copyright (c) 2017 Inhabited Type LLC.
 *  Copyright (c) 2019 Antonio N. Monteiro.
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the author nor the names of his contributors
 *     may be used to endorse or promote products derived from this software
 *     without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
 *  OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 *  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 *  DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
 *  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 *  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 *  OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 *  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 *  STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 *  ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

(** H2 is a high-performance, memory-efficient, and scalable HTTP/2
    implementation for OCaml. It is based on the concepts in http/af, and
    therefore uses the Angstrom and Faraday libraries to implement the parsing
    and serialization layers of the HTTP/2 standard. It also preserves the same
    API as http/af wherever possible.

    Not unlike http/af, the user should be familiar with HTTP, and the basic
    principles of memory management and vectorized IO in order to use this
    library. *)

(** {2 Basic HTTP Types} *)

(** Request Method

    The request method token is the primary source of request semantics; it
    indicates the purpose for which the client has made this request and what is
    expected by the client as a successful result.

    See {{:https://tools.ietf.org/html/rfc7231#section-4} RFC7231§4} for more
    details.

    This module is a proxy to [Httpaf.Method] and is included in h2 for
    convenience. *)
module Method : module type of Httpaf.Method

(** Response Status Codes

    The status-code element is a three-digit integer code giving the result of
    the attempt to understand and satisfy the request.

    See {{:https://tools.ietf.org/html/rfc7231#section-6} RFC7231§6} for more
    details.

    This module is a strict superset of [Httpaf.Status]. Even though the HTTP/2
    specification removes support for the [Switching_protocols] status code, h2
    keeps it for the sake of higher level interaction between OCaml libraries
    that support both HTTP/1 and HTTP/2.

    See {{:https://tools.ietf.org/html/rfc7540#section-8.1.1} RFC7540§8.1.1} for
    more details. *)
module Status : sig
  include
    module type of Httpaf.Status
      with type client_error := Httpaf.Status.client_error
       and type standard := Httpaf.Status.standard
       and type t := Httpaf.Status.t

  (** The 4xx (Client Error) class of status code indicates that the client
      seems to have erred.

      See {{:https://tools.ietf.org/html/rfc7231#section-6.5} RFC7231§6.5} for
      more details.

      In addition to http/af, this type also includes the 421 (Misdirected
      Request) tag. See {{:https://tools.ietf.org/html/rfc7540#section-9.1.2}
      RFC7540§9.1.2} for more details. *)
  type client_error =
    [ Httpaf.Status.client_error
    | `Misdirected_request
    ]

  (** The status codes defined in the HTTP/1.1 RFCs, excluding the
      [Switching Protocols] status and including the [Misdirected Request] as
      per the HTTP/2 RFC.

      See {{:https://tools.ietf.org/html/rfc7540#section-8.1.1} RFC7540§8.1.1}
      and {{:https://tools.ietf.org/html/rfc7540#section-9.1.2} RFC7540§9.1.2}
      for more details. *)
  type standard =
    [ Httpaf.Status.standard
    | client_error
    ]

  (** The standard codes along with support for custom codes. *)
  type t =
    [ standard
    | `Code of int
    ]

  val default_reason_phrase : standard -> string
  (** [default_reason_phrase standard] is the example reason phrase provided by
      RFC7231 for the [standard] status code. The RFC allows servers to use
      reason phrases besides these in responses. *)

  val to_code : t -> int
  (** [to_code t] is the integer representation of [t]. *)

  val of_code : int -> t
  (** [of_code i] is the [t] representation of [i]. [of_code] raises [Failure]
      if [i] is not a positive three-digit number. *)

  val unsafe_of_code : int -> t
  (** [unsafe_of_code i] is equivalent to [of_code i], except it accepts any
      positive code, regardless of the number of digits it has. On negative
      codes, it will still raise [Failure]. *)

  val is_informational : t -> bool
  (** [is_informational t] is [true] iff [t] belongs to the Informational class
      of status codes. *)

  val is_successful : t -> bool
  (** [is_successful t] is [true] iff [t] belongs to the Successful class of
      status codes. *)

  val is_redirection : t -> bool
  (** [is_redirection t] is [true] iff [t] belongs to the Redirection class of
      status codes. *)

  val is_client_error : t -> bool
  (** [is_client_error t] is [true] iff [t] belongs to the Client Error class of
      status codes. *)

  val is_server_error : t -> bool
  (** [is_server_error t] is [true] iff [t] belongs to the Server Error class of
      status codes. *)

  val is_error : t -> bool
  (** [is_server_error t] is [true] iff [t] belongs to the Client Error or
      Server Error class of status codes. *)

  val to_string : t -> string

  val of_string : string -> t

  val pp_hum : Format.formatter -> t -> unit
end

(** Header Fields

    Each header field consists of a lowercase {b field name} and a
    {b field value}. Per the HTTP/2 specification, header field names {b must}
    be converted to lowercase prior to their encoding in HTTP/2 (see
    {{:https://tools.ietf.org/html/rfc7540#section-8.1.2} RFC7540§8.1.2} for
    more details). h2 does {b not} convert field names to lowercase; it is
    therefore the responsibility of the caller of the functions contained in
    this module to use lowercase names for header fields.

    The order in which header fields {i with differing field names} are received
    is not significant, except for pseudo-header fields, which {b must} appear
    in header blocks before regular fields (see
    {{:https://tools.ietf.org/html/rfc7540#section-8.1.2.1} RFC7540§8.1.2.1} for
    more details).

    A sender MUST NOT generate multiple header fields with the same field name
    in a message unless either the entire field value for that header field is
    defined as a comma-separated list or the header field is a well-known
    exception, e.g., [Set-Cookie].

    A recipient MAY combine multiple header fields with the same field name into
    one "field-name: field-value" pair, without changing the semantics of the
    message, by appending each subsequent field value to the combined field
    value in order, separated by a comma.
    {i The order in which header fields with the same field name are received is
       therefore significant to the interpretation of the combined field value};
    a proxy MUST NOT change the order of these field values when forwarding a
    message.

    {i Note.} Unless otherwise specified, all operations preserve header field
    order and all reference to equality on names is assumed to be
    case-insensitive.

    See {{:https://tools.ietf.org/html/rfc7230#section-3.2} RFC7230§3.2} for
    more details. *)
module Headers : sig
  (** The type of a group of header fields. *)
  type t

  (** The type of a lowercase header name. *)
  type name = string

  (** The type of a header value. *)
  type value = string

  (** {3 Constructor} *)

  val empty : t
  (** [empty] is the empty collection of header fields. *)

  val of_list : (name * value) list -> t
  (** [of_list assoc] is a collection of header fields defined by the
      association list [assoc]. [of_list] assumes the order of header fields in
      [assoc] is the intended transmission order. The following equations should
      hold:

      - [to_list (of_list lst) = lst]
      - [get (of_list \[("k", "v1"); ("k", "v2")\]) "k" = Some "v2"]. *)

  val of_rev_list : (name * value) list -> t
  (** [of_list assoc] is a collection of header fields defined by the
      association list [assoc]. [of_list] assumes the order of header fields in
      [assoc] is the {i reverse} of the intended trasmission order. The
      following equations should hold:

      - [to_list (of_rev_list lst) = List.rev lst]
      - [get (of_rev_list \[("k", "v1"); ("k", "v2")\]) "k" = Some "v1"]. *)

  val to_list : t -> (name * value) list
  (** [to_list t] is the association list of header fields contained in [t] in
      transmission order. *)

  val to_rev_list : t -> (name * value) list
  (** [to_rev_list t] is the association list of header fields contained in [t]
      in {i reverse} transmission order. *)

  val add : t -> ?sensitive:bool -> name -> value -> t
  (** [add t ?sensitive name value] is a collection of header fields that is the
      same as [t] except with [(name, value)] added at the end of the
      trasmission order. Additionally, [sensitive] specifies whether this header
      field should not be compressed by HPACK and instead encoded as a
      never-indexed literal (see
      {{:https://tools.ietf.org/html/rfc7541#section-7.1.3} RFC7541§7.1.3} for
      more details).

      The following equations should hold:

      - [get (add t name value) name = Some value] *)

  val add_unless_exists : t -> ?sensitive:bool -> name -> value -> t
  (** [add_unless_exists t ?sensitive name value] is a collection of header
      fields that is the same as [t] if [t] already inclues [name], and
      otherwise is equivalent to [add t ?sensitive name value]. *)

  val add_list : t -> (name * value) list -> t
  (** [add_list t assoc] is a collection of header fields that is the same as
      [t] except with all the header fields in [assoc] added to the end of the
      transmission order, in reverse order. *)

  val add_multi : t -> (name * value list) list -> t
  (** [add_multi t assoc] is the same as

      {[
        add_list
          t
          (List.concat_map assoc ~f:(fun (name, values) ->
               List.map values ~f:(fun value -> name, value)))
      ]}

      but is implemented more efficiently. For example,

      {[
        add_multi t [ "name1", [ "x", "y" ]; "name2", [ "p", "q" ] ]
        = add_list [ "name1", "x"; "name1", "y"; "name2", "p"; "name2", "q" ]
      ]} *)

  val remove : t -> name -> t
  (** [remove t name] is a collection of header fields that contains all the
      header fields of [t] except those that have a header-field name that are
      equal to [name]. If [t] contains multiple header fields whose name is
      [name], they will all be removed. *)

  val replace : t -> ?sensitive:bool -> name -> value -> t
  (** [replace t ?sensitive name value] is a collection of header fields that is
      the same as [t] except with all header fields with a name equal to [name]
      removed and replaced with a single header field whose name is [name] and
      whose value is [value]. This new header field will appear in the
      transmission order where the first occurrence of a header field with a
      name matching [name] was found.

      If no header field with a name equal to [name] is present in [t], then the
      result is simply [t], unchanged. *)

  (** {3 Destructors} *)

  val mem : t -> name -> bool
  (** [mem t name] is [true] iff [t] includes a header field with a name that is
      equal to [name]. *)

  val get : t -> name -> value option
  (** [get t name] returns the last header from [t] with name [name], or [None]
      if no such header is present. *)

  val get_exn : t -> name -> value
  (** [get t name] returns the last header from [t] with name [name], or raises
      if no such header is present. *)

  val get_multi : t -> name -> value list
  (** [get_multi t name] is the list of header values in [t] whose names are
      equal to [name]. The returned list is in transmission order. *)

  (** {3 Iteration} *)

  val iter : f:(name -> value -> unit) -> t -> unit

  val fold : f:(name -> value -> 'a -> 'a) -> init:'a -> t -> 'a

  (** {3 Utilities} *)

  val to_string : t -> string

  val pp_hum : Format.formatter -> t -> unit
end

(** {2 Message Body} *)

module Body : sig
  type 'rw t

  val schedule_read
    :  [ `read ] t
    -> on_eof:(unit -> unit)
    -> on_read:(Bigstringaf.t -> off:int -> len:int -> unit)
    -> unit
  (** [schedule_read t ~on_eof ~on_read] will setup [on_read] and [on_eof] as
      callbacks for when bytes are available in [t] for the application to
      consume, or when the input channel has been closed and no further bytes
      will be received by the application.

      Once either of these callbacks have been called, they become inactive. The
      application is responsible for scheduling subsequent reads, either within
      the [on_read] callback or by some other mechanism. *)

  val write_char : [ `write ] t -> char -> unit
  (** [write_char w char] copies [char] into an internal buffer. If possible,
      this write will be combined with previous and/or subsequent writes before
      transmission. *)

  val write_string : [ `write ] t -> ?off:int -> ?len:int -> string -> unit
  (** [write_string w ?off ?len str] copies [str] into an internal buffer. If
      possible, this write will be combined with previous and/or subsequent
      writes before transmission. *)

  val write_bigstring
    :  [ `write ] t
    -> ?off:int
    -> ?len:int
    -> Bigstringaf.t
    -> unit
  (** [write_bigstring w ?off ?len bs] copies [bs] into an internal buffer. If
      possible, this write will be combined with previous and/or subsequent
      writes before transmission. *)

  val schedule_bigstring
    :  [ `write ] t
    -> ?off:int
    -> ?len:int
    -> Bigstringaf.t
    -> unit
  (** [schedule_bigstring w ?off ?len bs] schedules [bs] to be transmitted at
      the next opportunity without performing a copy. [bs] should not be
      modified until a subsequent call to {!flush} has successfully completed. *)

  val flush : [ `write ] t -> (unit -> unit) -> unit
  (** [flush t f] makes all bytes in [t] available for writing to the awaiting
      output channel. Once those bytes have reached that output channel, [f]
      will be called.

      The type of the output channel is runtime-dependent, as are guarantees
      about whether those packets have been queued for delivery or have actually
      been received by the intended recipient. *)

  val close_reader : [ `read ] t -> unit
  (** [close_reader t] closes [t], indicating that any subsequent input received
      should be discarded. *)

  val close_writer : [ `write ] t -> unit
  (** [close_writer t] closes [t], causing subsequent write calls to raise. If
      [t] is writable, this will cause any pending output to become available to
      the output channel. *)

  val is_closed : _ t -> bool
  (** [is_closed t] is [true] if {!close} has been called on [t] and [false]
      otherwise. A closed [t] may still have pending output. *)
end

(** {2 Message Types} *)

(** Request

    A client-initiated HTTP message. *)
module Request : sig
  type t =
    { meth : Method.t
    ; target : string
    ; scheme : string
    ; headers : Headers.t
    }

  val create
    :  ?headers:Headers.t (** default is {!Headers.empty} *)
    -> scheme:string
    -> Method.t
    -> string
    -> t
  (** [create ?headers ~scheme meth target] creates an HTTP request with the
      given parameters. In HTTP/2, the [:scheme] pseudo-header field is required
      and includes the scheme portion of the target URI. The [headers] parameter
      is optional, however clients will want to include the [:authority]
      pseudo-header field in most cases. The [:authority] pseudo-header field
      includes the authority portion of the target URI, and should be used
      instead of the [Host] header field in HTTP/2.

      See {{:https://tools.ietf.org/html/rfc7540#section-8.1.2.3}
      RFC7540§8.1.2.4} for more details. *)

  val body_length
    :  t
    -> [ `Error of [ `Bad_request ] | `Fixed of int64 | `Unknown ]
  (** [body_length t] is the length of the message body accompanying [t].

      See {{:https://tools.ietf.org/html/rfc7230#section-3.3.3} RFC7230§3.3.3}
      for more details. *)

  val pp_hum : Format.formatter -> t -> unit
end

(** Response

    A server-generated message to a {!Request.t}. *)
module Response : sig
  type t =
    { status : Status.t
    ; headers : Headers.t
    }

  val create
    :  ?headers:Headers.t (** default is {!Headers.empty} *)
    -> Status.t
    -> t
  (** [create ?headers status] creates an HTTP response with the given
      parameters. Unlike the [Response] type in http/af, h2 does not define a
      way for responses to carry reason phrases or protocol version.

      See {{:https://tools.ietf.org/html/rfc7540#section-8.1.2.4}
      RFC7540§8.1.2.4} for more details. *)

  val body_length
    :  t
    -> [ `Error of [ `Bad_request ] | `Fixed of int64 | `Unknown ]
  (** [body_length t] is the length of the message body accompanying [t].

      See {{:https://tools.ietf.org/html/rfc7230#section-3.3.3} RFC7230§3.3.3}
      for more details. *)

  val pp_hum : Format.formatter -> t -> unit
end

(** IOVec *)
module IOVec : module type of Httpaf.IOVec

(** {2 Request Descriptor} *)
module Reqd : sig
  type t

  val request : t -> Request.t

  val request_body : t -> [ `read ] Body.t

  val response : t -> Response.t option

  val response_exn : t -> Response.t

  (** {3 Responding}

      The following functions will initiate a response for the corresponding
      request in [t]. When the response is fully transmitted to the wire, the
      stream completes.

      From {{:https://tools.ietf.org/html/rfc7540#section-8.1} RFC7540§8.1}: An
      HTTP request/response exchange fully consumes a single stream. *)

  val respond_with_string : t -> Response.t -> string -> unit

  val respond_with_bigstring : t -> Response.t -> Bigstringaf.t -> unit

  val respond_with_streaming
    :  t
    -> ?flush_headers_immediately:bool
    -> Response.t
    -> [ `write ] Body.t

  val schedule_trailers : t -> Headers.t -> unit
  (** [schedule_trailers reqd trailers] schedules a list of trailers to be sent
      before the stream is closed, concluding the HTTP message. Should only be
      used after {!respond_with_streaming}. Raises [Failure] if trailers have
      already been scheduled. See
      {{:https://tools.ietf.org/html/rfc7540#section-8.1} RFC7540§8.1} for more
      information *)

  (** {3 Pushing}

      HTTP/2 allows a server to pre-emptively send (or "push") responses (along
      with corresponding "promised" requests) to a client in association with a
      previous client-initiated request. This can be useful when the server
      knows the client will need to have those responses available in order to
      fully process the response to the original request.

      {4 {b An additional note regarding server push:}}

      In HTTP/2, PUSH_PROMISE frames must only be sent in the open or
      half-closed ("remote") stream states. In practice, this means that calling
      {!Reqd.push} must happen before the entire response body for the
      associated client-initiated request has been written to the wire. As such,
      it is dangerous to start a server pushed response in association with
      either {!Reqd.respond_with_string} or {!Reqd.respond_with_bigstring}, as
      the entire body for the response that they produce is sent to the output
      channel immediately, causing the corresponding stream to enter the closed
      state.

      See {{:https://tools.ietf.org/html/rfc7540#section-8.2} RFC7540§8.2} for
      more details. *)

  val push
    :  t
    -> Request.t
    -> ( t
       , [ `Push_disabled | `Stream_cant_push | `Stream_ids_exhausted ] )
       result
  (** [push reqd request] creates a new ("pushed") request descriptor that
      allows responding to the "promised" [request]. As per the HTTP/2
      specification, [request] must be cacheable, safe, and must not include a
      request body (see {{:https://tools.ietf.org/html/rfc7540.html#section-8.2}
      RFC7540§8.2} for more details). {b Note}: h2 will not validate [request]
      against these assumptions.

      This function returns [Error `Push_disabled] when the value of
      [SETTINGS_ENABLE_PUSH] is set to [0] (see
      {{:https://tools.ietf.org/html/rfc7540.html#section-6.5.2} RFC7540§8.2}
      for more details), [Error `Stream_cant_push] when trying to initiate a
      push stream from a stream that has been obtained from pushing, or
      [Error `Stream_ids_exhausted] when the connection has exhausted the range
      of identifiers available for pushed streams and cannot push on that
      connection anymore. *)

  (** {3 Exception Handling} *)

  val report_exn : t -> exn -> unit

  val try_with : t -> (unit -> unit) -> (unit, exn) result
end

(** {2 Errors} *)
module Error_code : sig
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

  val to_string : t -> string

  val pp_hum : Format.formatter -> t -> unit
end

(* TODO: needs docs *)
module Settings : sig
  type t =
    { header_table_size : int
    ; enable_push : bool
    ; max_concurrent_streams : int32
    ; initial_window_size : int32
    ; max_frame_size : int
    ; max_header_list_size : int option
    }

  val default : t

  val of_base64 : string -> (t, string) result
  (** {{:https://tools.ietf.org/html/rfc7540#section-3.2.1} RFC7540§3.2.1} *)

  val to_base64 : t -> (string, string) result
  (** {{:https://tools.ietf.org/html/rfc7540#section-3.2.1} RFC7540§3.2.1} *)

  val pp_hum : Format.formatter -> t -> unit
end

(** {2 HTTP/2 Configuration} *)
module Config : sig
  type t =
    { read_buffer_size : int
          (** [read_buffer_size] specifies the size of the largest frame payload
              that the sender is willing to receive, in octets. Defaults to
              [16384] *)
    ; request_body_buffer_size : int  (** Defaults to [4096] *)
    ; response_body_buffer_size : int  (** Defaults to [4096] *)
    ; enable_server_push : bool  (** Defaults to [true] *)
    ; max_concurrent_streams : int32
          (** [max_concurrent_streams] specifies the maximum number of streams
              that the sender will allow the peer to initiate. Defaults to
              [2^31 - 1] *)
    ; initial_window_size : int32
          (** [initial_window_size] specifies the initial window size for flow
              control tokens. Defaults to [65535] *)
    }

  val default : t
  (** [default] is a configuration record with all parameters set to their
      default values. *)

  val to_settings : t -> Settings.t
end

(** {2 Server Connection} *)

module Server_connection : sig
  type t

  type error =
    [ `Bad_request
    | `Internal_server_error
    | `Exn of exn
    ]

  type request_handler = Reqd.t -> unit

  type error_handler =
    ?request:Request.t -> error -> (Headers.t -> [ `write ] Body.t) -> unit

  val create
    :  ?config:Config.t
    -> ?error_handler:error_handler
    -> request_handler
    -> t
  (** [create ?config ?error_handler ~request_handler] creates a connection
      handler that will service individual requests with [request_handler]. *)

  val create_h2c
    :  ?config:Config.t
    -> ?error_handler:error_handler
    -> http_request:Httpaf.Request.t
    -> ?request_body:Bigstringaf.t IOVec.t list
    -> request_handler
    -> (t, string) result
  (** [create ?config ?error_handler ~http_request ~request_handler] creates a
      connection handler that will take over the communication channel from a
      HTTP/1.1 connection, and service individual HTTP/2.0 requests with
      [request_handler]. Upon successful creation, it returns the connection,
      otherwise an error message is returned with an explanation of the failure
      that caused the connection setup to not succeed.

      This function is intended to be used in HTTP/1.1 upgrade handlers to set
      up a new [h2c] (HTTP/2.0 over TCP) connection without prior knowledge.

      See {{:https://tools.ietf.org/html/rfc7540#section-3.2} RFC7540§3.2} for
      more details. *)

  val next_read_operation : t -> [> `Read | `Close ]
  (** [next_read_operation t] returns a value describing the next operation that
      the caller should conduct on behalf of the connection. *)

  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      value and additional input is available for the connection to consume. *)

  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      and an EOF has been received from the communication channel. The
      connection will attempt to consume any buffered input and then shutdown
      the HTTP parser for the connection. *)

  val next_write_operation
    :  t
    -> [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {!report_write_result} should be called after a
      call to {!next_write_operation} that returns a [`Write buffer] value.

      - [`Ok n] indicates that the caller successfully wrote [n] bytes of output
        from the buffer that the caller was provided by {!next_write_operation}.
      - [`Closed] indicates that the output destination will no longer accept
        bytes from the write processor. *)

  val yield_writer : t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call [continue]
      when writing should resume. {!yield_writer} should be called after
      {!next_write_operation} returns a [`Yield] value. *)

  val yield_reader : t -> (unit -> unit) -> unit
  (** [yield_reader t continue] immediately calls [continue]. This function *
      shouldn't generally be called and it's only here to simplify adhering * to
      the Gluten [RUNTIME] module type. *)

  val report_exn : t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and that it
      has been attributed to [t]. Calling this function will switch [t] into an
      error state. Depending on the state [t] is transitioning from, it may call
      its error handler before terminating the connection. *)

  val is_closed : t -> bool
  (** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close _] and {!next_write_operation} will do the same will return a
      [`Write _] until all buffered output has been flushed, at which point it
      will return [`Close]. *)

  (* val error_code : t -> error option *)
  (** [error_code t] returns the [error_code] that caused the connection to
      close, if one exists. *)

  (**/**)

  val shutdown : t -> unit

  (**/**)
end

(** {2 Client Connection} *)

module Client_connection : sig
  type t

  type error =
    [ `Malformed_response of string
    | `Invalid_response_body_length of Response.t
    | `Protocol_error of Error_code.t * string
    | `Exn of exn
    ]

  type trailers_handler = Headers.t -> unit

  type response_handler = Response.t -> [ `read ] Body.t -> unit

  type error_handler = error -> unit

  val create
    :  ?config:Config.t
    -> ?push_handler:(Request.t -> (response_handler, unit) result)
    -> error_handler:error_handler
    -> t
  (** [create ?config ?push_handler ~error_handler] creates a connection that
      can be used to interact with servers over the HTTP/2 protocol.

      [error_handler] will be called for {e connection-level} errors. HTTP/2 is
      multiplexed over a single TCP connection and distinguishes
      connection-level errors from stream-level errors. See See
      {{:https://tools.ietf.org/html/rfc7540#section-5.4} RFC7540§5.4} for more
      details.

      If present, [push_handler] will be called upon the receipt of PUSH_PROMISE
      frames with the request promised by the server. This function should
      return [Ok response_handler] if the client wishes to accept the pushed
      request. In this case, [response_handler] will be called once the server
      respondes to the pushed request. Returning [Error ()] will signal to h2
      that the client is choosing to reject the request that the server is
      pushing, and its stream will be closed, as per the following excerpt from
      the HTTP/2 specification:

      From RFC7540§6.6: Recipients of PUSH_PROMISE frames can choose to reject
      promised streams by returning a RST_STREAM referencing the promised stream
      identifier back to the sender of the PUSH_PROMISE. *)

  val create_h2c
    :  ?config:Config.t
    -> ?push_handler:(Request.t -> (response_handler, unit) result)
    -> http_request:Httpaf.Request.t
    -> error_handler:error_handler
    -> response_handler * error_handler
    -> (t, string) result

  val request
    :  t
    -> ?trailers_handler:trailers_handler
    -> Request.t
    -> error_handler:error_handler
    -> response_handler:response_handler
    -> [ `write ] Body.t
  (** [request connection ?trailers_handler req ~error_handler
      ~response_handler]
      opens a new HTTP/2 stream with [req] and returns a request body that can
      be written to. Once a response arrives, [response_handler] will be called
      with its headers and body. [error_handler] will be called for
      {e stream-level} errors. If there are any trailers they will be parsed and
      passed to [trailers_handler].

      HTTP/2 is multiplexed over a single TCP connection and distinguishes
      connection-level errors from stream-level errors. See
      {{:https://tools.ietf.org/html/rfc7540#section-5.4} RFC7540§5.4} for more
      details. *)

  val ping : t -> ?payload:Bigstringaf.t -> ?off:int -> (unit -> unit) -> unit
  (** [ping connection ?payload ?off f] sends an HTTP/2 PING frame and registers
      [f] to be called when the server has sent an acknowledgement for it. A
      custom [payload] (and offset into that payload) for the PING frame may
      also be provided. If not, a payload with all bytes set to zero will be
      used. Note that a PING frame's payload {b must} be 8 octets in length.

      In HTTP/2, the PING frame is a mechanism for measuring a minimal
      round-trip time from the sender, as well as determining whether an idle
      connection is still functional. See
      {{:https://tools.ietf.org/html/rfc7540#section-5.4} RFC7540§5.4} for more
      details. *)

  val shutdown : t -> unit
  (** [shutdown connection] initiates the graceful shutdown of [connection], and
      sends an HTTP/2 GOAWAY frame with NO_ERROR on the output channel (See
      {{:https://tools.ietf.org/html/rfc7540#section-6.8} RFC7540§6.8} for more
      details). *)

  val next_read_operation : t -> [> `Read | `Close ]
  (** [next_read_operation t] returns a value describing the next operation that
      the caller should conduct on behalf of the connection. *)

  val read : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      value and additional input is available for the connection to consume. *)

  val read_eof : t -> Bigstringaf.t -> off:int -> len:int -> int
  (** [read t bigstring ~off ~len] reads bytes of input from the provided range
      of [bigstring] and returns the number of bytes consumed by the connection.
      {!read} should be called after {!next_read_operation} returns a [`Read]
      and an EOF has been received from the communication channel. The
      connection will attempt to consume any buffered input and then shutdown
      the HTTP parser for the connection. *)

  val next_write_operation
    :  t
    -> [ `Write of Bigstringaf.t IOVec.t list | `Yield | `Close of int ]
  (** [next_write_operation t] returns a value describing the next operation
      that the caller should conduct on behalf of the connection. *)

  val report_write_result : t -> [ `Ok of int | `Closed ] -> unit
  (** [report_write_result t result] reports the result of the latest write
      attempt to the connection. {!report_write_result} should be called after a
      call to {!next_write_operation} that returns a [`Write buffer] value.

      - [`Ok n] indicates that the caller successfully wrote [n] bytes of output
        from the buffer that the caller was provided by {!next_write_operation}.
      - [`Closed] indicates that the output destination will no longer accept
        bytes from the write processor. *)

  val yield_writer : t -> (unit -> unit) -> unit
  (** [yield_writer t continue] registers with the connection to call [continue]
      when writing should resume. {!yield_writer} should be called after
      {!next_write_operation} returns a [`Yield] value. *)

  val yield_reader : t -> (unit -> unit) -> unit
  (** [yield_reader t continue] immediately calls [continue]. This function *
      shouldn't generally be called and it's only here to simplify adhering * to
      the Gluten [RUNTIME] module type. *)

  val report_exn : t -> exn -> unit
  (** [report_exn t exn] reports that an error [exn] has been caught and that it
      has been attributed to [t]. Calling this function will switch [t] into an
      error state. Depending on the state [t] is transitioning from, it may call
      its (connection-level) error handler before terminating the connection. *)

  val is_closed : t -> bool
  (** [is_closed t] is [true] if both the read and write processors have been
      shutdown. When this is the case {!next_read_operation} will return
      [`Close _] and {!next_write_operation} will do the same will return a
      [`Write _] until all buffered output has been flushed, at which point it
      will return [`Close]. *)
end
