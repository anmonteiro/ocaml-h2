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

type nothing = [ `Tls_not_available ]

module Io :
  H2_lwt.IO
    with type socket = Lwt_unix.file_descr * nothing
     and type addr = Unix.sockaddr = struct
  type socket = Lwt_unix.file_descr * nothing

  type addr = Unix.sockaddr

  let read _ _bigstring ~off:_ ~len:_ = Lwt.fail_with "Tls not available"

  let writev _ _iovecs = Lwt.fail_with "Tls not available"

  let shutdown_send _ = failwith "Tls not available"

  let shutdown_receive _ = failwith "Tls not available"

  let close _ = Lwt.fail_with "Tls not available"

  let report_exn _connection _ _exn = Lwt.fail_with "Tls not available"
end

type client = nothing

type server = nothing

let[@ocaml.warning "-21"] make_client ?client:_ =
  failwith "TLS not available";
  fun _socket -> Lwt.return `Tls_not_available

let[@ocaml.warning "-21"] make_server ?server:_ ?certfile:_ ?keyfile:_ =
  failwith "TLS not available";
  fun _socket -> Lwt.fail_with "TLS not available"
