Unreleased
-----------

- h2: Fix tests on 32-bit platforms
  ([#152](https://github.com/anmonteiro/ocaml-h2/pull/152))
- h2-mirage: adapt to Conduit v4 and remove `H2_mirage.Server_with_conduit`
  ([#154](https://github.com/anmonteiro/ocaml-h2/pull/154))

0.8.0 2021-04-11
---------------

- h2: scheduler: fix bug that caused zero length DATA frames not to be sent if
  there were no flow-control credits
  ([#142](https://github.com/anmonteiro/ocaml-h2/pull/142)) -- reported  by
  [@blandinw](https://github.com/blandinw)
- h2,h2-lwt,h2-lwt-unix,h2-async,h2-mirage Add `trailers_handler` to
  `Connection.request` ([#146](https://github.com/anmonteiro/ocaml-h2/pull/146))
- h2: client / server: fix a security issue that allowed a malicious peer to
  make h2 allocate as much as it wanted
  ([#149](https://github.com/anmonteiro/ocaml-h2/pull/149))

0.7.0 2020-08-09
---------------

- h2: client / server: execute request/response body reads as data frames
  arrive ([#130](https://github.com/anmonteiro/ocaml-h2/pull/130))
- h2: client / server: only give back flow control tokens after surfacing reads
  to the application ([#131](https://github.com/anmonteiro/ocaml-h2/pull/131))
- h2: Set a default of 128MiB for the initial receiving window size
  ([#132](https://github.com/anmonteiro/ocaml-h2/pull/132))
- h2: don't attempt to write frames to an encoder that has closed
  ([#134](https://github.com/anmonteiro/ocaml-h2/pull/134))
- h2: Handle frame size errors in a more robust manner when parsing
  ([#133](https://github.com/anmonteiro/ocaml-h2/pull/133))
- h2: Create push streams with the right priority (as per
  [RFC7540§5.3.5](https://tools.ietf.org/html/rfc7540#section-5.3.5), pushed
  streams initially depend on their associated stream)
  ([#136](https://github.com/anmonteiro/ocaml-h2/pull/136))

0.6.1 2020-05-16
---------------
- h2-async: add Async adapter
  ([#94](https://github.com/anmonteiro/ocaml-h2/pull/94))
- h2-async: Use [gluten](https://github.com/anmonteiro/gluten) to implement
  h2-async ([#125](https://github.com/anmonteiro/ocaml-h2/pull/125))
- h2-mirage: Use [gluten](https://github.com/anmonteiro/gluten) to implement
  h2-mirage ([#120](https://github.com/anmonteiro/ocaml-h2/pull/120))
- h2: Don't put parser in error state when force-closing
  ([#127](https://github.com/anmonteiro/ocaml-h2/pull/127))

0.6.0 2020-04-29
--------------

- h2-lwt: Close the communication channel after shutting down the client
  ([#108](https://github.com/anmonteiro/ocaml-h2/pull/108))
- h2-lwt-unix: fix premature SSL termination in the SSL / TLS runtimes
  ([#109](https://github.com/anmonteiro/ocaml-h2/pull/109))
- h2-lwt-unix: TLS runtime: adapt to TLS v0.11.0
  ([#109](https://github.com/anmonteiro/ocaml-h2/pull/109))
- h2-lwt-unix: feed EOF to the state machine if the socket has been closed --
  this is especially important on the client because it allows connections to
  terminate cleanly. ([#112](https://github.com/anmonteiro/ocaml-h2/pull/112))
- h2: Refactor the `Settings` module API
  ([#113](https://github.com/anmonteiro/ocaml-h2/pull/113))
- h2-lwt, h2-lwt-unix: Use [gluten](https://github.com/anmonteiro/gluten) to
  implement the Lwt-based runtimes
  ([#114](https://github.com/anmonteiro/ocaml-h2/pull/114))
- h2: set a lower bound on Angstrom 0.14.0
  ([#118](https://github.com/anmonteiro/ocaml-h2/pull/118))
- h2: in the client implementation, don't report an error if the server has
  sent an `RST_STREAM` frame after sending a complete response
  ([#119](https://github.com/anmonteiro/ocaml-h2/pull/119)).
- h2-lwt-unix: fix a regression that prevented the SSL / TLS runtimes to
  negotiate an HTTP/2 connection over the ALPN extension of TLS, in their
  default implementations
  ([#122](https://github.com/anmonteiro/ocaml-h2/pull/122))

0.5.0 2019-12-19
--------------

- h2, h2-lwt, h2-lwt-unix, h2-mirage: Remove support for versions of OCaml
  lower than 4.06 ([#74](https://github.com/anmonteiro/ocaml-h2/pull/74))
- h2: Expose more information in client error handlers when initiating a
  connection ([#80](https://github.com/anmonteiro/ocaml-h2/pull/80))
- h2: Make H2.Status.t a strict superset of Httpaf.Status.t
  ([#83](https://github.com/anmonteiro/ocaml-h2/pull/83))
- h2-lwt, h2-lwt-unix: split HTTPS functions in 2: one that sets up a default
  secure connection and performs the TLS handshake / accept, and one that is
  more "raw", i.e. leaves that responsibility to the caller. Also exposes the
  `socket` type to make it easier to abstract over HTTP / HTTPS
  ([#84](https://github.com/anmonteiro/ocaml-h2/pull/84))
- h2-lwt, h2-lwt-unix, h2-mirage: Improve the `H2_lwt.IO` interface, don't
  require a `report_exn` function, only a `state` function that returns the
  socket state ([#85](https://github.com/anmonteiro/ocaml-h2/pull/85))
- h2, h2-lwt, h2-lwt-unix, h2-mirage: Add support for starting HTTP/2 for
  "http" URIs. Covers [section 3.2](https://tools.ietf.org/html/rfc7540#section-3.2)
  of the HTTP/2 specification
  ([#87](https://github.com/anmonteiro/ocaml-h2/pull/87))
- h2: Fix misinterpretation of the spec where h2 would consider a request /
  response malformed if it had a non-zero `content-length` header and no DATA
  frames ([#89](https://github.com/anmonteiro/ocaml-h2/pull/89))
- h2: Add `Request.body_length` and `Response.body_length`
  ([#90](https://github.com/anmonteiro/ocaml-h2/pull/90))
- h2: Fix a bug that caused DATA frames to be incorrectly chunked when
  returning a streaming response
  ([#91](https://github.com/anmonteiro/ocaml-h2/pull/91))
- h2: Drain pending bytes after getting a `Close` report from the runtime
  ([#92](https://github.com/anmonteiro/ocaml-h2/pull/92))
- h2: Report connection errors for unknown frames that exceed the maximum
  payload size -- they may not be speaking HTTP/2
 ([#93](https://github.com/anmonteiro/ocaml-h2/pull/93))

0.4.0 2019-11-05
--------------

- h2-mirage: depend on `mirage-conduit` instead of `conduit-mirage`,
  effectively placing a lower bound of OCaml 4.07 on the next release of
  h2-mirage ([#67](https://github.com/anmonteiro/ocaml-h2/pull/67))
- h2-lwt-unix: replace the `dune` file (previously written in OCaml) with a
  `(select)` form to avoid depending on `ocamlfind`
  ([#68](https://github.com/anmonteiro/ocaml-h2/pull/68))
- h2-lwt, h2-lwt-unix, h2-mirage: Refactor interface code through common
  `H2_lwt_intf` and expose less (internal) types
  ([#65](https://github.com/anmonteiro/ocaml-h2/pull/65))
- h2-lwt, h2-lwt-unix, h2-mirage: Expose `Client.is_closed`
  ([#65](https://github.com/anmonteiro/ocaml-h2/pull/65))
- h2: Don't count peer max concurrent streams based on what the endpoint
  receives; the endpoint is responsible for selecting it
  ([#71](https://github.com/anmonteiro/ocaml-h2/pull/71))
- h2: Fix bug in `Headers.remove` that prevented removing the last header pair
  ([#73](https://github.com/anmonteiro/ocaml-h2/pull/73))
- h2: Fix bug in `Headers.replace` that prevented replacing the last header
  pair ([#76](https://github.com/anmonteiro/ocaml-h2/pull/76))
- h2-mirage: Adapt to Mirage 3.7 interfaces. `h2_mirage` now requires
  `conduit-mirage` >= 2.0.2 and `mirage-flow` >= 2.0.0
  ([#77](https://github.com/anmonteiro/ocaml-h2/pull/77))

0.3.0 2019-05-04
--------------

- h2-mirage: Provide `Server` and `Client` functors that take a
  `Mirage_flow_lwt.S` module as an argument
  ([#37](https://github.com/anmonteiro/ocaml-h2/pull/37))
- h2: Fix bug in the client implementation that didn't report connection
  preface errors as soon as they happened
  ([#38](https://github.com/anmonteiro/ocaml-h2/pull/38))
- h2: optimize the stream scheduler: previously when the writer yielded between
  writes, a wake up function was registered with all the (active) streams,
  which required a linear traversal of all the streams. The optimization is to
  allow every stream to wake up a global writer to which they hold a reference
  ([#40](https://github.com/anmonteiro/ocaml-h2/pull/40))
- h2: improve handling of received frames against closed streams
  ([#40](https://github.com/anmonteiro/ocaml-h2/pull/40))
- h2: in the client implementation, call the stream level error handler when
  receiving an `RST_STREAM` frame
  ([#42](https://github.com/anmonteiro/ocaml-h2/pull/42))
- h2-lwt-unix: fail earlier when setting up a SSL/TLS server without the
  depopts being available
  ([#46](https://github.com/anmonteiro/ocaml-h2/pull/46))
- h2-lwt-unix: improve the default ALPN negotiation mechanism in the SSL
  binding ([#46](https://github.com/anmonteiro/ocaml-h2/pull/46))

0.2.0 2019-04-06
--------------

- h2: Fix false negative related to receiving trailer headers with CONTINUATION
  frames ([#11](https://github.com/anmonteiro/ocaml-h2/pull/11))
- hpack: Fix bug where trying to add an entry to an HPACK dynamic table with 0
  capacity resulted in an out-of-bounds array access
  ([#13](https://github.com/anmonteiro/ocaml-h2/pull/13),
  [#35](https://github.com/anmonteiro/ocaml-h2/pull/35))
- h2: Add support for the 421 (Misdirected Request) status code as per
  [RFC7540§9.1.2](https://tools.ietf.org/html/rfc7540#section-9.1.2)
  ([#15](https://github.com/anmonteiro/ocaml-h2/pull/15))
- h2, h2-lwt, h2-lwt-unix, h2-mirage: Add an HTTP/2 client implementation
  ([#16](https://github.com/anmonteiro/ocaml-h2/pull/16))
- h2: Remove dependency on the `result` package
  ([#18](https://github.com/anmonteiro/ocaml-h2/pull/18))
- h2: Track SETTINGS frames that haven't been acknowledged by the peer
  ([#22](https://github.com/anmonteiro/ocaml-h2/pull/22))
- h2: Don't treat `CONNECT` requests as malformed
  ([#32](https://github.com/anmonteiro/ocaml-h2/pull/32),
  [#34](https://github.com/anmonteiro/ocaml-h2/pull/34))
- h2: Respect the initial MAX\_FRAME\_SIZE setting when allocating the
  underlying buffer for the frame writer
  ([#34](https://github.com/anmonteiro/ocaml-h2/pull/34))

0.1.0 2019-03-27
--------------

- Initial public release
