Unreleased
--------------

- h2: Fix false negative related to receiving trailer headers with CONTINUATION
  frames ([#11](https://github.com/anmonteiro/ocaml-h2/pull/11))
- hpack: Fix bug where trying to add an entry to an HPACK dynamic table with 0
  capacity resulted in an out-of-bounds array access
  ([#13](https://github.com/anmonteiro/ocaml-h2/pull/13))
- h2: Add support for the 421 (Misdirected Request) status code as per
  [RFC7540§9.1.2](https://tools.ietf.org/html/rfc7540#section-9.1.2)
  ([#15](https://github.com/anmonteiro/ocaml-h2/pull/15))
- h2, h2-lwt, h2-lwt-unix, h2-mirage: Add an HTTP/2 client implementation
  ([#16](https://github.com/anmonteiro/ocaml-h2/pull/16))
- h2: Remove dependency on the `result` package
  ([#18](https://github.com/anmonteiro/ocaml-h2/pull/18))

0.1.0 2019-03-27
--------------

- Initial public release
