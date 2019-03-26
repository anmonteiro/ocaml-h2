# http2/af

http2/af is an implementation of the
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
opam install http2af
```
## Performance

http2/af aims to be a high-performance, memory-efficient, scalable, and easily
portable (with respect to different I/O runtimes) implementation. To achieve
that, it takes advantage of the unbuffered parsing interface in Angstrom using
off-heap buffers wherever possible, for both parsing and serialization.

## Limitations

http2/af only currently provides server implementation. In the future, a client
library will also be provided.

## Usage

TBD, see the examples folder for now.

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
  core http2/af package.
-  [`lwt/`](./lwt): contains an implementation of a Lwt runtime for http2/af
  functorized over the specific input / output channel abstraction such that it
  can work in either UNIX-like systems or MirageOS.
- [`lwt-unix/`](./lwt-unix): contains an Lwt runtime adapter for http2/af that
  communicates over UNIX file descriptors.
- [`mirage/`](./mirage): contains a Mirage runtime adapter for http2/af that
  allows using http2/af to write unikernels that serve traffic over HTTP/2.
- [`spec/`](./spec): contains example implementations of servers using http2/af
  that respond with the different provided APIs to be used for conformance
  testing with the [h2spec](https://github.com/summerwind/h2spec) tool.

### Using OPAM

To install development dependencies, pin the package from the root of the
repository:

```bash
$ opam pin add -n hpack .
$ opam pin add -n http2af .
$ opam install --deps-only http2af
```

After this, you may install a development version of the library using the
install command as usual.

Tests can be run via dune:

```bash
dune runtest
```

### Using [Esy](https://esy.sh)

There's an `esy.json` file at the root of this repository that can be used
to develop http2/af with Esy.

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

BSD3, see LICENSE files for its text.


Configs TODO:

- related to above: disable flow control as a config?
  - Deployments that do not require this capability can advertise a flow-control window of the maximum size (2^31-1) and can maintain this window by sending a WINDOW_UPDATE frame when any data is received. This effectively disables flow control for that receiver.
- GOAWAY frames: keep state that we've received a goaway frame
- send a goaway frame when stream ids are exhausted
  - Stream identifiers cannot be reused. Long-lived connections can result in an endpoint exhausting the available range of stream identifiers. A client that is unable to establish a new stream identifier can establish a new connection for new streams. A server that is unable to establish a new stream identifier can send a GOAWAY frame so that the client is forced to open a new connection for new streams.
- design available settings (config)
  - push_enabled
  - max concurrent streams
  - max frame size
  - size for header block buffers?
  - initial window size
- client
- send the max concurrent streams setting such that h2spec can test it.

