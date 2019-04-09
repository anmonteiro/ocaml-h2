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

### Resources

First of all, the generated documentation lives
[here](https://anmonteiro.github.io/ocaml-h2/). It is recommended to browse it
and get to know the API exposed by `H2`.

There are also some examples in the [`examples`](./examples) folder. Most
notably, the [ALPN example](./examples/alpn) provides an implementation of a
common real-world use case:

It sets up a server that listens on 2 ports:

1. __port 8080__: redirects all incoming traffic to `https://localhost:9443`
2. __port 9443__: negotiates which protocol to use over the TLS
   [Application-Layer Protocol Negotiation
   (ALPN)](https://tools.ietf.org/html/rfc7301) extension. It supports 2
   protocols (in order of preference): `h2` and `http/1.1`.

    If `h2` is negotiated, the example sets up a connection handler using
    `h2-lwt-unix`. Otherwise the connection handler will serve HTTP/1.1 traffic
    using http/af.

The ALPN example also provides a unikernel implementation with the same
functionality that runs on [MirageOS](https://mirage.io).

### A simplistic (server) example

We present an annotated example below that responds to any `GET` request and
returns a response body containing the target of the request.

```ocaml
open H2

(* This is our request handler. H2 will invoke this function whenever the
 * client send a request to our server. *)
let request_handler _client_address reqd =
  (* `reqd` is a "request descriptor". Conceptually, it's just a reference to
   * the request that the client sends, which allows us to do two things:
   *
   * 1. Get more information about the request that we're handling. In our
   *    case, we're inspecting the method and the target of the request, but we
   *    could also look at the request headers.
   *
   * 2. A request descriptor is also what allows us to respond to this
   *    particular request by passing it into one of the response functions
   *    that we will look at below. *)
  let { Request.meth; target; _ } = Reqd.request reqd in
  match meth with
  | `GET ->
    let response_body =
      Printf.sprintf
        "You made a request to the following resource: %s\n"
        target
    in
    (* Specify the length of the response body. Two notes to make here:
     *
     * 1. Specifying the content length of a response is optional since HTTP/2
     *    is a binary protocol based on frames which carry information about
     *    whether a frame is the last for a given stream.
     *
     * 2. In HTTP/2, all header names are required to be lowercase. We use
     *    `content-length` instead of what might be commonly seen in HTTP/1.X
     *    (`Content-Length`). *)
    let headers =
      Headers.of_list
        [ "content-length", string_of_int (String.length response_body) ]
    in
    (* Respond immediately with the response body we constructed above,
     * finishing the request/response exchange (and the unerlying HTTP/2
     * stream).
     *
     * The other functions in the `Reqd` module that allow sending a response
     * to the client are `Reqd.respond_with_bigstring`, that only differs from
     * `Reqd.respond_with_string` in that the response body should be a
     * bigarray, and `Reqd.respond_with_streaming` (see
     * http://anmonteiro.com/ocaml-h2/h2/H2/Reqd/index.html#val-respond_with_streaming)
     * which starts streaming a response body which can be written to
     * asynchronously to the client. *)
    Reqd.respond_with_string reqd (Response.create ~headers `OK) response_body
  | meth ->
    let response_body =
      Printf.sprintf
        "This server does not respond to %s methods.\n"
        (Method.to_string meth)
    in
    Reqd.respond_with_string
      reqd
      (* We don't include any headers in this case. The HTTP/2 framing layer
       * knows that these will be last frames in the exchange. *)
      (Response.create `Method_not_allowed)
      response_body

(* This is our error handler. Everytime H2 sees a malformed request or an
 * exception on a specific stream, it will invoke this function to send a
 * response back to the misbehaving client. Because there might not be a
 * request for the stream (handing malformed requests to the application is
 * strongly discouraged), there is also no request descriptor like we saw in
 * the request handler above. In this case, one of the arguments to this
 * function is a function that will start the response. It has the following
 * signature:
 *
 *   val start_response : H2.headers.t -> [`write] H2.Body.t
 *
 * This is also where we first encounter the concept of a `Body` (which were
 * briefly mentioned above) that can be written to (potentially
 * asynchronously). *)
let error_handler _client_address ?request:_ error start_response =
  (* We start the error response by calling the `start_response` function. We
   * get back a response body. *)
  let response_body = start_response Headers.empty in
  (* Once we get the response body, we can immediately start writing to it. In
   * this case, it might be sufficient to say that there was an error. *)
  Body.write_string response_body "There was an error handling your request.\n";
  (* Finally, we close the streaming response body to signal to the underlying
   * HTTP/2 framing layer that we have finished sending the response. *)
  Body.close_writer response_body

let () =
  (* We're going to be using the `H2_lwt_unix` module from the `h2-lwt-unix`
   * library to create a server that communicates over the underlying operating
   * system socket abstraction. The first step is to create a connection
   * handler that will accept incoming connections and let our request and
   * error handlers handle the request / response exchanges in those
   * connections. *)
  let connection_handler =
    H2_lwt_unix.Server.create_connection_handler
      ?config:None
      ~request_handler
      ~error_handler
  in
  (* We'll be listening for requests on the loopback interface (localhost), on
   * port 8080. *)
  let listen_address = Unix.(ADDR_INET (inet_addr_loopback, 8080)) in
  (* The final step is to start a server that will set up all the low-level
   * networking communication for us, and let it run forever. *)
  Lwt.async (fun () ->
      Lwt_io.establish_server_with_client_socket
        listen_address
        connection_handler);
  let forever, _ = Lwt.wait () in
  Lwt_main.run forever
```

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

Below is a plot of H2's latency profile at a sustained rate of 17000 requests
per second over 30 seconds, benchmarked using the
[vegeta](https://github.com/tsenart/vegeta) load testing tool.

<img src="./vegeta-plot.png" alt="ocaml-h2 req/s plot" />

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
# Use --recurse-submodules to get the test git submodules
$ git clone git@github.com:anmonteiro/ocaml-h2.git --recurse-submodules
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

h2 is distributed under the 3-Clause BSD License, see [LICENSE](./LICENSE).

This source distribution includes work based on
[http/af](https://github.com/inhabitedtype/httpaf). http/af's license file is
included in [httpaf.LICENSE](./httpaf.LICENSE)
