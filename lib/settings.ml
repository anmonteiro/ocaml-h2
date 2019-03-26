module WindowSize = struct
  type t = int

  (* From RFC7540§6.9.2:
       When an HTTP/2 connection is first established, new streams are created
       with an initial flow-control window size of 65,535 octets. *)
  let default_initial_window_size = 65535

  (* From RFC7540§6.9:
       The legal range for the increment to the flow-control window is 1 to
       2^31-1 (2,147,483,647) octets. *)
  let max_window_size = 1 lsl 31 - 1

  (* Ideally `n` here would be an unsigned 32-bit integer, but OCaml doesn't
   * support them. We avoid introducing a new dependency on an unsigned integer
   * library by letting it overflow at parse time and checking if bit 31 is set
   * here, since * `Window.max_window_size` is never allowed to be above
   * 2^31-1 (see `max_window_size` above). *)
  let is_window_overflow n = Util.test_bit n 31
end

type key =
  | HeaderTableSize
  | EnablePush
  | MaxConcurrentStreams
  | InitialWindowSize
  | MaxFrameSize (* this means payload size *)
  | MaxHeaderListSize

type value = int

type settings_list = (key * value) list

let from_key = function
  | HeaderTableSize -> 0x1
  | EnablePush -> 0x2
  | MaxConcurrentStreams -> 0x3
  | InitialWindowSize -> 0x4
  | MaxFrameSize -> 0x5
  | MaxHeaderListSize -> 0x6

let to_key = function
  | 0x1 -> Some HeaderTableSize
  | 0x2 -> Some EnablePush
  | 0x3 -> Some MaxConcurrentStreams
  | 0x4 -> Some InitialWindowSize
  | 0x5 -> Some MaxFrameSize
  | 0x6 -> Some MaxHeaderListSize
  | _ -> None

let check_value = function
  | EnablePush, v when v != 0 && v != 1 ->
    (* From RFC7540§6.5.2
         The initial value is 1, which indicates that server push is permitted.
         Any value other than 0 or 1 MUST be treated as a connection error
         (Section 5.4.1) of type PROTOCOL_ERROR. *)
    Some Error.(ConnectionError (ProtocolError, "SETTINGS_ENABLE_PUSH must be 0 or 1"))
  | InitialWindowSize, v when WindowSize.is_window_overflow v ->
    (* From RFC7540§6.5.2
         Values above the maximum flow-control window size of 2^31-1 MUST be
         treated as a connection error (Section 5.4.1) of type
         FLOW_CONTROL_ERROR. *)
    Some
      Error.(ConnectionError
        ( FlowControlError
        , Format.sprintf "Window size must be less than or equal to %d"
            WindowSize.max_window_size))
  | MaxFrameSize, v when v < 16384 || v > 16777215 ->
    (* From RFC7540§6.5.2
         The initial value is 214 (16,384) octets. The value advertised by an
         endpoint MUST be between this initial value and the maximum allowed
         frame size (224-1 or 16,777,215 octets), inclusive. Values outside
         this range MUST be treated as a connection error (Section 5.4.1) of
         type PROTOCOL_ERROR. *)
    Some Error.(ConnectionError (ProtocolError, "Max frame size must be in the 16384 - 16777215 range"))
  | _ -> None

(* Check incoming settings and report an error if any. *)
let rec check_settings_list = function
  | [] -> None
  | x::xs ->
    begin match check_value x with
    | None -> check_settings_list xs
    | Some _ as err -> err
    end

type t =
  { mutable header_table_size : int
  ; mutable enable_push : bool
  ; mutable max_concurrent_streams : int option
  ; mutable initial_window_size : int
  ; mutable max_frame_size : int
  ; mutable max_header_list_size : int option
  }

(* From RFC7540§11.3 *)
let default_settings =
  { header_table_size = 0x1000
  ; enable_push = true
  ; max_concurrent_streams = None
  ; initial_window_size = WindowSize.default_initial_window_size
  ; max_frame_size = 0x4000
  ; max_header_list_size = None
  }

