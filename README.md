# h2

h2 is an implementation of the
[HTTP/2](https://tools.ietf.org/html/rfc7540) specification entirely in OCaml.
It is based on the concepts in
[http/af](https://github.com/inhabitedtype/httpaf), and therefore uses the
[Angstrom][angstrom] and [Faraday][faraday] libraries to implement the parsing
and serialization layers of the HTTP/2 standard. It also preserves the same API
as http/af wherever possible.

[angstrom]: https://github.com/inhabitedtype/angstrom
[faraday]: https://github.com/inhabitedtype/faraday

## Installation

Install the library and its dependencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install h2
```

## Usage

TBD, see the [`examples`](./examples) folder and the [`.mli`
file](./lib/h2.mli) for now.

## Conformance

One of h2's goals is to be 100% compliant with the HTTP/2 specification.
There are currently 3 mechanisms in place to verify such conformance:

1. Unit tests using the HPACK stories in the
   [http2jp/hpack-test-case](https://github.com/http2jp/hpack-test-case).
   repository
2. Unit tests using the test cases provided by the
   [http2jp/http2-frame-test-case](https://github.com/http2jp/http2-frame-test-case)
   repository.
3. Automated test runs (in CI) using the
   [h2spec](https://github.com/summerwind/h2spec) conformance testing tool for
   HTTP/2 implementations.
   - These test all the `Reqd.respond_with_*` functions for conformance against
     the specification.

## Performance

h2 aims to be a high-performance, memory-efficient, scalable, and easily
portable (with respect to different I/O runtimes) implementation. To achieve
that, it takes advantage of the unbuffered parsing interface in Angstrom using
off-heap buffers wherever possible, for both parsing and serialization.

## Limitations

h2 only currently provides a server implementation. In the future, a
client library will also be provided.

## Development

This source distribution provides a number of packages and examples. The
directory structure is as follows:

- [`examples/`](./examples): contains example applications using the various
  I/O runtimes provided in this source distribution.
- [`hpack/`](./hpack): contains the implementation of
  [HPACK](https://tools.ietf.org/html/rfc7541), the Header Compression
  specification for HTTP/2.
- [`lib/`](./lib): contains the core implementation of this library, including
  HTTP/2 frame parsing, serialization and state machine implementations.
- [`lib_test/`](./lib_test): contains various unit tests for modules in the
  core h2 package.
-  [`lwt/`](./lwt): contains an implementation of a Lwt runtime for h2
  functorized over the specific input / output channel abstraction such that it
  can work in either UNIX-like systems or MirageOS.
- [`lwt-unix/`](./lwt-unix): contains an Lwt runtime adapter for h2 that
  communicates over UNIX file descriptors.
- [`mirage/`](./mirage): contains a Mirage runtime adapter for h2 that
  allows using h2 to write unikernels that serve traffic over HTTP/2.
- [`spec/`](./spec): contains example implementations of servers using h2
  that respond with the different provided APIs to be used for conformance
  testing with the [h2spec](https://github.com/summerwind/h2spec) tool.

### Cloning the repository

```shell
# Clone the repository
$ git clone git@github.com:anmonteiro/h2.git
# cd into the directory and clone the submodules (http2 standard test cases)
$ cd h2
$ git submodule update --init --recursive
```

### Using OPAM

To install development dependencies, pin the package from the root of the
repository:

```bash
$ opam pin add -n hpack .
$ opam pin add -n h2 .
$ opam install --deps-only h2
```

After this, you may install a development version of the library using the
install command as usual.

Tests can be run via dune:

```bash
dune runtest
```

### Using [Esy](https://esy.sh)

There's an `esy.json` file at the root of this repository that can be used
to develop h2 with Esy.

To resolve and install the necessary dependencies, run:

```
$ esy
```

Build any examples or run the tests by prefixing the `dune` commands with
`esy b` or `esy build`. For example:

```
$ esy b dune runtest
```

## License

Distributed under the 3-Clause BSD License, see [LICENSE](./LICENSE).

This source distribution includes work based on
[http/af](https://github.com/inhabitedtype/httpaf). http/af's license file is
included in [httpaf.LICENSE](./httpaf.LICENSE)
