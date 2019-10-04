Unreleased
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
