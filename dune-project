(lang dune 2.7)

(name h2)

(formatting
 (enabled_for ocaml))

(generate_opam_files true)

(source
 (github anmonteiro/ocaml-h2))

(authors "Antonio Nuno Monteiro <anmonteiro@gmail.com>")

(maintainers "Antonio Nuno Monteiro <anmonteiro@gmail.com>")

(homepage "https://github.com/anmonteiro/ocaml-h2")

(bug_reports "https://github.com/anmonteiro/ocaml-h2/issues")

(license "BSD-3-clause")

(package
 (name hpack)
 (synopsis
  "An HPACK (Header Compression for HTTP/2) implementation in OCaml")
 (description
  "hpack is an implementation of the HPACK: Header Compression for HTTP/2 specification (RFC7541) written in OCaml. It uses Angstrom and Faraday for parsing and serialization, respectively.")
 (depends
  (ocaml
   (>= "4.08.0"))
  angstrom
  (faraday
   (>= "0.7.3"))
  (yojson :with-test)
  (hex :with-test)))

(package
 (name h2)
 (synopsis
  "A high-performance, memory-efficient, and scalable HTTP/2 library for OCaml")
 (description
  "h2 is an implementation of the HTTP/2 specification entirely in OCaml. It is based on the concepts in http/af, and therefore uses the Angstrom and Faraday libraries to implement the parsing and serialization layers of the HTTP/2 standard as a state machine that is agnostic to the underlying I/O specifics. It also preserves the same API as http/af wherever possible.")
 (depends
  (ocaml
   (>= "4.08.0"))
  (base64
   (>= "3.0.0"))
  (angstrom
   (>= "0.14.0"))
  (faraday
   (>= "0.7.3"))
  (bigstringaf
   (>= "0.5.0"))
  psq
  (hpack (= :version))
  httpaf
  (alcotest :with-test)
  (yojson :with-test)
  (hex :with-test)))

(package
 (name h2-lwt)
 (synopsis "Lwt support for h2")
 (description
  "h2 is an implementation of the HTTP/2 specification entirely in OCaml. h2-lwt provides an Lwt runtime implementation for h2.")
 (depends
  (ocaml
   (>= "4.08.0"))
  (h2
   (= :version))
  (lwt
   (>= "5.1.1"))
  (gluten-lwt
   (>= "0.2.1"))))

(package
 (name h2-mirage)
 (synopsis "Lwt support for h2")
 (description
  "h2 is an implementation of the HTTP/2 specification entirely in OCaml. h2-mirage provides an Lwt runtime implementation for h2 that targets MirageOS unikernels.")
 (depends
  (ocaml
   (>= "4.08.0"))
  (h2-lwt
   (= :version))
  faraday-lwt
  lwt
  (gluten-mirage
   (>= "0.3.0"))
  (mirage-flow
   (>= "2.0.0"))
  cstruct))

(package
 (name h2-lwt-unix)
 (synopsis "Lwt + UNIX support for h2")
 (description
  "h2 is an implementation of the HTTP/2 specification entirely in OCaml. h2-lwt-unix provides an Lwt runtime implementation for h2 that targets UNIX binaries.")
 (depends
  (ocaml
   (>= "4.08.0"))
  (h2-lwt
   (= :version))
  faraday-lwt-unix
  (gluten-lwt-unix
   (>= "0.2.1")))
 (depopts tls-lwt lwt_ssl))

(package
 (name h2-async)
 (synopsis "Async support for h2")
 (description
  "h2 is an implementation of the HTTP/2 specification entirely in OCaml. h2-async provides an Async runtime implementation for h2.")
 (depends
  (ocaml
   (>= "4.08.0"))
  (h2
   (= :version))
  faraday-async
  (gluten-async
   (>= "0.4.0")))
 (depopts async_ssl tls-async))

(package
 (name h2-eio)
 (synopsis "EIO support for h2")
 (description
  "h2 is an implementation of the HTTP/2 specification entirely in OCaml. h2-eio provides an EIO runtime implementation for h2.")
 (depends
  (ocaml
   (>= "4.08.0"))
  (h2
   (= :version))
  (gluten-eio
   (>= "0.4.1"))))
