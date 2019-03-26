type header =
  { name : string
  ; value : string
    (* From RFC7541§7.1.3:
         Implementations can also choose to protect sensitive header fields by
         not compressing them and instead encoding their value as literals. *)
  ; sensitive : bool
  }

type error =
  | Decoding_error

module Encoder : sig
  type t

  val create : int -> t
  (** [create capacity] initializes an encoder with a dynamic table with
      maximum size [capacity]. This size is an approximation of the memory
      overhead in bytes.

      See {{:https://tools.ietf.org/html/rfc7541#section-4.1} RFC7541§4.1} for
      more details. *)

  val encode_header : t -> Faraday.t -> header -> unit
  (** [encode_header encoder f header] writes an encoded header to the Faraday
      buffer [f]. *)

  val set_capacity : t -> int -> unit
  (** [set_capacity encoder capacity] sets [encoder]'s dynamic table size to
      maximum size [capacity]. This size is an approximation of the memory
      overhead in bytes.

      See {{:https://tools.ietf.org/html/rfc7540#section-6.5.2} RFC7540§6.5.2}
      and {{:https://tools.ietf.org/html/rfc7541#section-4.1} RFC7541§4.1} for
      more details. *)

end

module Decoder : sig
  type t

  val create : int -> t
  (** [create capacity] initializes a decoder with a dynamic table with maximum
      size [capacity]. This size is an approximation of the memory usage in
      bytes.

      See {{:https://tools.ietf.org/html/rfc7541#section-4.1} RFC7541§4.1} for
      more details. *)

  val set_capacity : t -> int -> (unit, error) result
  (** [set_capacity decoder capacity] sets [decoder]'s dynamic table size to
      maximum size [capacity]. This size is an approximation of the memory
      overhead in bytes.

      See {{:https://tools.ietf.org/html/rfc7540#section-6.5.2} RFC7540§6.5.2}
      and {{:https://tools.ietf.org/html/rfc7541#section-4.1} RFC7541§4.1} for
      more details. *)

  val decode_headers : t -> (header list, error) result Angstrom.t
  (** [decode_headers decoder] creates an Angstrom parser that will decode a
      header block and return a list of the decoded headers *)
end
