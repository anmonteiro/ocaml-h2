type t =
  { mutable entries : (string * string * int) array
  ; mutable length : int
  ; mutable offset : int
  ; mutable capacity : int
    (* `length` above is the number of entries in the dynamic table. We track
       the HPACK size in `size`.

       From RFC7541§4.1:
         The size of the dynamic table is the sum of the size of its entries.

         The size of an entry is the sum of its name's length in octets (as
         defined in Section 5.2), its value's length in octets, and 32. *)
  ; mutable size : int
    (* From RFC7541§4.2:
         Protocols that use HPACK determine the maximum size that the encoder
         is permitted to use for the dynamic table. In HTTP/2, this value is
         determined by the SETTINGS_HEADER_TABLE_SIZE setting (see Section
         6.5.2 of [HTTP2]). *)
  ; mutable max_size : int
  ; on_evict : (string * string) -> unit
  }

(* From RFC7541§4.1:
     The size of an entry is the sum of its name's length in octets (as defined
     in Section 5.2), its value's length in octets, and 32. *)
let default_entry = ("", "", 32)
let default_evict = Sys.opaque_identity (fun _ -> ())

let create ?(on_evict=default_evict) max_size =
  let capacity = max 256 max_size in
  { entries = Array.make capacity default_entry
  ; length = 0
  ; offset = 0
  ; capacity
  ; size = 0
  ; max_size
  ; on_evict
  }

let[@inline] _get table i =
  table.entries.((table.offset + i) mod table.capacity)

let[@inline] get table i =
  let name, value, _ = _get table i in
  name, value

let[@inline] entry_size name value =
  (* From RFC7541§4.1:
       The size of an entry is the sum of its name's length in octets (as
       defined in Section 5.2), its value's length in octets, and 32. *)
  String.length name + String.length value + 32

let evict_one ({ capacity; entries; on_evict; _ } as table) =
  table.length <- table.length - 1;
  let i = (table.offset + table.length) mod capacity in
  let (name, value, entry_size) = entries.(i) in
  entries.(i) <- default_entry;
  table.size <- table.size - entry_size;
  (* Don't bother calling if the eviction callback is not meaningful. *)
  if on_evict != default_evict then
    on_evict (name, value)

let increase_capacity table =
  let new_capacity = 2 * table.capacity in
  let new_entries = Array.init new_capacity (fun i ->
    if i < table.length then
      _get table i
    else
      default_entry)
  in
  table.entries <- new_entries;
  table.offset <- 0;
  table.capacity <- new_capacity

let add ({ max_size; _ } as table) (name, value) =
  let entry_size = entry_size name value in
  (* From RFC7541§4.4:
       Before a new entry is added to the dynamic table, entries are evicted
       from the end of the dynamic table until the size of the dynamic table is
       less than or equal to (maximum size - new entry size) or until the table
       is empty. *)
  while table.size + entry_size > max_size do
    evict_one table
  done;
  (* From RFC7541§4.4:
       If the size of the new entry is less than or equal to the maximum size,
       that entry is added to the table. *)
  if table.size + entry_size <= max_size then begin
    if table.length = table.capacity then
      increase_capacity table;
    table.length <- table.length + 1;
    table.size <- table.size + entry_size;
    let new_offset = (table.offset + table.capacity - 1) mod table.capacity in
    table.entries.(new_offset) <- name, value, entry_size;
    table.offset <- new_offset;
  end

let[@inline] table_size table = table.length

let set_capacity table max_size =
  table.max_size <- max_size;
  (* From RFC7541§4.3:
       Whenever the maximum size for the dynamic table is reduced, entries are
       evicted from the end of the dynamic table until the size of the dynamic
       table is less than or equal to the maximum size. *)
  while table.size > max_size do
    evict_one table
  done

