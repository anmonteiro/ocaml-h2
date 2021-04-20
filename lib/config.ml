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

(* TODO: add a config option for `Reqd` to flush max bytes at a time? *)
type t =
  { read_buffer_size : int
  ; request_body_buffer_size : int
  ; response_body_buffer_size : int
  ; enable_server_push : bool
  ; max_concurrent_streams : int32
  ; initial_window_size : int32
  }

let default =
  { (* This is effectively MAX_FRAME_SIZE, because the parser commits the frame
     * header to prevent backtracking, therefore the entire payload can fit the
     * read buffer. The default is 16384, and can't be lower than that.
     *
     * Note: h2 does not check that MAX_FRAME_SIZE is lower than 16384
     * octets. In the case that a lower value than permitted is set, peers will
     * reject the setting and close the connection with a PROTOCOL_ERROR.
     *
     * From RFC7540§6.5.2:
     *   SETTINGS_MAX_FRAME_SIZE (0x5): Indicates the size of the largest frame
     *   payload that the sender is willing to receive, in octets.
     *   The initial value is 2^14 (16,384) octets. The value advertised by an
     *   endpoint MUST be between this initial value and the maximum allowed
     *   frame size (2^24-1 or 16,777,215 octets), inclusive. *)
    read_buffer_size = Settings.default.max_frame_size
  ; (* Buffer size for request bodies *) request_body_buffer_size = 0x1000
  ; (* Buffer size for response bodies *) response_body_buffer_size = 0x1000
  ; enable_server_push = true
  ; (* From RFC7540§6.5.2:
     *   Indicates the maximum number of concurrent streams that the sender
     *   will allow. This limit is directional: it applies to the number of
     *   streams that the sender permits the receiver to create. *)
    max_concurrent_streams = Settings.default.max_concurrent_streams
  ; (* Indicates the initial window size when receiving data from remote
     * streams. In other words, represents the amount of octets that the H2
     * endpoint is willing to receive from the peer. Cannot be lower than
     * 65535 (the default as per the spec). The default in H2 is 2^27, or
     * 128 MiB. *)
    (* TODO(anmonteiro): validate the default somewhere. *)
    initial_window_size = Int32.shift_left 1l 27
  }

let to_settings
    { read_buffer_size
    ; max_concurrent_streams
    ; initial_window_size
    ; enable_server_push
    ; _
    }
  =
  { Settings.default with
    max_frame_size = read_buffer_size
  ; max_concurrent_streams
  ; initial_window_size
  ; enable_push = enable_server_push
  }
