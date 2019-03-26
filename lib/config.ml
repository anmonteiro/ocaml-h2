type t =
  { read_buffer_size          : int
  ; request_body_buffer_size  : int
  ; response_buffer_size      : int
  ; response_body_buffer_size : int
  ; enable_server_push        : bool
  ; max_concurrent_streams    : int
  ; initial_window_size       : int
  }

let default =
  { (* This is effectively MAX_FRAME_SIZE, because the parser commits the frame
       header to prevent backtracking, therefore the entire payload can fit the
       read buffer. The default is 16384, and can't be lower than that.

       Note: http2/af does not check that MAX_FRAME_SIZE is lower than 16384
       octets. In the case that a lower value than permitted is set, peers will
       reject the setting and close the connection with a PROTOCOL_ERROR.

       From RFC7540§6.5.2:
         SETTINGS_MAX_FRAME_SIZE (0x5): Indicates the size of the largest frame
         payload that the sender is willing to receive, in octets.
         The initial value is 2^14 (16,384) octets. The value advertised by an
         endpoint MUST be between this initial value and the maximum allowed
         frame size (2^24-1 or 16,777,215 octets), inclusive. *)
    read_buffer_size          = Settings.default_settings.max_frame_size
  ; request_body_buffer_size  = 0x1000
    (* The buffer size for the response writer *)
  ; response_buffer_size      = 0x400
    (* The buffer size for response bodies *)
  ; response_body_buffer_size = 0x1000
  ; enable_server_push        = true
   (* From RFC7540§6.5.2:
        Indicates the maximum number of concurrent streams that the sender will
        allow. This limit is directional: it applies to the number of streams
        that the sender permits the receiver to create. *)
  ; max_concurrent_streams    = Settings.default_settings.max_concurrent_streams
   (* From RFC7540§6.5.2:
        Indicates the sender's initial window size (in octets) for stream-level
        flow control. *)
  ; initial_window_size = Settings.WindowSize.default_initial_window_size
  }

