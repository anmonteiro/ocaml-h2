Unreleased
--------------

- h2: Fix false negative related to receiving trailer headers with CONTINUATION
  frames ([#11](https://github.com/anmonteiro/ocaml-h2/pull/11))
- hpack: Fix bug where trying to add an entry to an HPACK dynamic table with 0
  capacity resulted in an out-of-bounds array access
  ([#13](https://github.com/anmonteiro/ocaml-h2/pull/13))

0.1.0 2019-03-27
--------------

- Initial public release
