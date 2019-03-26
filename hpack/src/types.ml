type header =
  { name : string
  ; value : string
  ; sensitive : bool
  }

type error =
  | Decoding_error

