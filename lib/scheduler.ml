(*----------------------------------------------------------------------------
 *  Copyright (c) 2019 António Nuno Monteiro
 *
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions are met:
 *
 *  1. Redistributions of source code must retain the above copyright notice,
 *  this list of conditions and the following disclaimer.
 *
 *  2. Redistributions in binary form must reproduce the above copyright
 *  notice, this list of conditions and the following disclaimer in the
 *  documentation and/or other materials provided with the distribution.
 *
 *  3. Neither the name of the copyright holder nor the names of its
 *  contributors may be used to endorse or promote products derived from this
 *  software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 *  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 *  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 *  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 *  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 *  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 *  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 *  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 *  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 *  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 *  POSSIBILITY OF SUCH DAMAGE.
 *---------------------------------------------------------------------------*)

module StreamsTbl = struct
  include Hashtbl.MakeSeeded (struct
    type t = Stream_identifier.t

    let equal = Stream_identifier.( === )

    let hash i k = Hashtbl.seeded_hash i k
  end)

  let[@inline] find_opt h key = try Some (find h key) with Not_found -> None
end

module type StreamDescriptor = sig
  type t

  val id : t -> Stream_identifier.t

  val requires_output : t -> bool

  val flush_write_body : t -> max_bytes:int -> int

  val finish_stream : t -> Stream.closed_reason -> unit

  val is_idle : t -> bool
end

module Make (Streamd : StreamDescriptor) = struct
  module rec PriorityTreeNode : sig
    type root = Root

    type nonroot = NonRoot

    type stream = nonroot node

    and parent = Parent : _ node -> parent

    and _ node =
      (* From RFC7540§5.3.1:
       *   A stream that is not dependent on any other stream is given a stream
       *   dependency of 0x0. In other words, the non-existent stream 0 forms
       *   the root of the tree.
       *
       * Note:
       *   We use a GADT because the root of the tree doesn't have an
       *   associated request descriptor. It has the added advantage of
       *   allowing us to enforce that all (other) streams in the tree are
       *   associated with a request descriptor. *)
      | Connection :
          { all_streams : stream StreamsTbl.t
          ; mutable t_last : int
          ; mutable children : PriorityQueue.t
                (* Connection-level flow control window.
                 * outbound flow control, what we're allowed to send.
                 *
                 * From RFC7540§6.9.1:
                 *   Two flow-control windows are applicable: the stream
                 *   flow-control window and the connection flow-control
                 *   window. *)
          ; mutable flow : Settings.WindowSize.t
                (* inbound flow control, what the client is allowed to send. *)
          ; mutable inflow : Settings.WindowSize.t
          ; mutable marked_for_removal :
              (Stream_identifier.t * Stream.closed) list
          }
          -> root node
      | Stream :
          { descriptor : Streamd.t
          ; mutable t_last : int
          ; mutable t : int
          ; mutable priority : Priority.t
          ; mutable parent : parent
          ; mutable children : PriorityQueue.t
                (* Stream-level flow control window. See connection-level above.
                 *
                 * From RFC7540§6.9.1:
                 *   Two flow-control windows are applicable: the stream
                 *   flow-control window and the connection flow-control
                 *   window. *)
          ; mutable flow : Settings.WindowSize.t
          ; mutable inflow : Settings.WindowSize.t
          }
          -> nonroot node
  end =
    PriorityTreeNode

  and PriorityQueue :
    (Psq.S with type k = Int32.t and type p = PriorityTreeNode.stream) =
    Psq.Make
      (Int32)
      (struct
        include PriorityTreeNode

        type t = stream

        let compare (Stream { t = t1; _ }) (Stream { t = t2; _ }) =
          compare t1 t2
      end)

  include PriorityTreeNode

  type t = root node

  (* TODO(anmonteiro): change according to SETTINGS_MAX_CONCURRENT_STREAMS? *)
  let make_root ?(capacity = 65536) () =
    Connection
      { t_last = 0
      ; children = PriorityQueue.empty
      ; all_streams = StreamsTbl.create ~random:true capacity
      ; flow = Settings.WindowSize.default_initial_window_size
      ; inflow = Settings.WindowSize.default_initial_window_size
      ; marked_for_removal = []
      }

  let create
      ~parent ~initial_send_window_size ~initial_recv_window_size descriptor
    =
    Stream
      { descriptor
      ; t_last = 0
      ; t =
          0
          (* From RFC7540§5.3.5:
           *   All streams are initially assigned a non-exclusive dependency on
           *   stream 0x0. Pushed streams (Section 8.2) initially depend on
           *   their associated stream. In both cases, streams are assigned a
           *   default weight of 16. *)
      ; priority = Priority.default_priority
      ; parent
      ; children = PriorityQueue.empty
      ; flow = initial_send_window_size
      ; inflow = initial_recv_window_size
      }

  let pq_add stream_id node pq = PriorityQueue.add stream_id node pq

  let remove_from_parent (Parent parent) id =
    match parent with
    | Connection root ->
      (* From RFC7540§5.3.1:
       *   A stream that is not dependent on any other stream is given a stream
       *   dependency of 0x0. In other words, the non-existent stream 0 forms
       *   the root of the tree. *)
      root.children <- PriorityQueue.remove id root.children
    | Stream stream ->
      stream.children <- PriorityQueue.remove id stream.children

  let children : type a. a node -> PriorityQueue.t = function
    | Stream { children; _ } ->
      children
    | Connection { children; _ } ->
      children

  let stream_id : type a. a node -> int32 = function
    | Connection _ ->
      Stream_identifier.connection
    | Stream { descriptor; _ } ->
      Streamd.id descriptor

  let set_parent stream_node ~exclusive new_parent =
    let (Stream ({ descriptor; _ } as stream)) = stream_node in
    let (Parent new_parent_node) = new_parent in
    let stream_id = Streamd.id descriptor in
    remove_from_parent stream.parent stream_id;
    stream.parent <- new_parent;
    let new_children =
      let new_children = children new_parent_node in
      if exclusive then (
        (* From RFC7540§5.3.3:
         *   Dependent streams move with their parent stream if the parent is
         *   reprioritized. Setting a dependency with the exclusive flag for a
         *   reprioritized stream causes all the dependencies of the new parent
         *   stream to become dependent on the reprioritized stream. *)
        stream.children <-
          PriorityQueue.fold
            (fun k (Stream p as p_node) pq ->
              p.parent <- Parent stream_node;
              PriorityQueue.add k p_node pq)
            stream.children
            new_children;
        (* From RFC7540§5.3.1:
         *   An exclusive flag allows for the insertion of a new level of
         *   dependencies. The exclusive flag causes the stream to become the
         *   sole dependency of its parent stream, causing other dependencies
         *   to become dependent on the exclusive stream. *)
        PriorityQueue.sg stream_id stream_node)
      else
        pq_add stream_id stream_node new_children
    in
    match new_parent_node with
    | Stream stream ->
      stream.children <- new_children
    | Connection root ->
      root.children <- new_children

  let would_create_cycle ~new_parent (Stream { descriptor; _ }) =
    let rec inner : type a. a node -> bool = function
      | Connection _ ->
        false
      | Stream { parent = Parent parent; _ }
        when Stream_identifier.(stream_id parent === Streamd.id descriptor) ->
        true
      | Stream { parent = Parent parent; _ } ->
        inner parent
    in
    let (Parent parent_node) = new_parent in
    inner parent_node

  let reprioritize_stream (Connection root as t) ~priority stream_node =
    let (Stream stream) = stream_node in
    let new_parent, new_priority =
      if Stream_identifier.is_connection priority.Priority.stream_dependency
      then
        Parent t, priority
      else
        match
          StreamsTbl.find_opt root.all_streams priority.stream_dependency
        with
        | Some parent_stream ->
          Parent parent_stream, priority
        | None ->
          (* From RFC7540§5.3.1:
           *   A dependency on a stream that is not currently in the tree —
           *   such as a stream in the "idle" state — results in that stream
           *   being given a default priority (Section 5.3.5). *)
          Parent t, Priority.default_priority
    in
    (* bail early if trying to set the same priority *)
    if not (Priority.equal stream.priority new_priority) then (
      let { Priority.stream_dependency; exclusive; _ } = new_priority in
      let (Parent current_parent_node) = stream.parent in
      let current_parent_id = stream_id current_parent_node in
      (* only need to set a different parent if the parent or exclusive status
       * changed *)
      if
        (not Stream_identifier.(stream_dependency === current_parent_id))
        || exclusive <> stream.priority.exclusive
      then (
        let (Parent new_parent_node) = new_parent in
        (match new_parent_node with
        | Stream new_parent_stream ->
          if would_create_cycle ~new_parent stream_node then (
            (* From RFC7540§5.3.3:
             *   If a stream is made dependent on one of its own dependencies,
             *   the formerly dependent stream is first moved to be dependent
             *   on the reprioritized stream's previous parent. The moved
             *   dependency retains its weight. *)
            set_parent new_parent_node ~exclusive:false stream.parent;
            new_parent_stream.priority <-
              { new_parent_stream.priority with
                stream_dependency = current_parent_id
              })
        | Connection _ ->
          (* The root node cannot be dependent on any other streams, so we
           * don't need to worry about it creating cycles. *)
          ());
        (* From RFC7540§5.3.1:
         *   When assigning a dependency on another stream, the stream is added
         *   as a new dependency of the parent stream. *)
        set_parent stream_node ~exclusive new_parent);
      stream.priority <- priority)

  let add
      (Connection root as t)
      ~priority
      ~initial_send_window_size
      ~initial_recv_window_size
      descriptor
    =
    let stream =
      create
        ~parent:(Parent t)
        ~initial_send_window_size
        ~initial_recv_window_size
        descriptor
    in
    let stream_id = Streamd.id descriptor in
    StreamsTbl.add root.all_streams stream_id stream;
    root.children <- pq_add stream_id stream root.children;
    if priority != Priority.default_priority then
      reprioritize_stream t ~priority stream;
    stream

  let get_node (Connection root) stream_id =
    StreamsTbl.find_opt root.all_streams stream_id

  let find t stream_id =
    match get_node t stream_id with
    | Some (Stream { descriptor; _ }) ->
      Some descriptor
    | None ->
      None

  let iter (Connection { all_streams; _ }) ~f =
    StreamsTbl.iter (fun _id -> f) all_streams

  let allowed_to_transmit (Connection root) (Stream stream) =
    Int32.compare root.flow 0l > 0 && Int32.compare stream.flow 0l > 0

  let allowed_to_receive (Connection root) (Stream stream) size =
    size <= root.inflow && size <= stream.inflow

  let write (Connection root as t) stream_node =
    let (Stream ({ descriptor; _ } as stream)) = stream_node in
    (* From RFC7540§6.9.1:
     *   Two flow-control windows are applicable: the stream flow-control
     *   window and the connection flow-control window. The sender MUST NOT
     *   send a flow-controlled frame with a length that exceeds the space
     *   available in either of the flow-control windows advertised by the
     *   receiver. *)
    let allowed_bytes =
      if allowed_to_transmit t stream_node then
        min root.flow stream.flow
      else
        (* There might be a zero-length DATA frame (with the end stream flag
           set) waiting to be sent. *)
        0l
    in
    let written =
      Int32.of_int
      @@ Streamd.flush_write_body
           ~max_bytes:(Int32.to_int allowed_bytes)
           descriptor
    in
    (* From RFC7540§6.9.1:
     *   After sending a flow-controlled frame, the sender reduces the space
     *   available in both windows by the length of the transmitted frame. *)
    root.flow <- Int32.sub root.flow written;
    stream.flow <- Int32.sub stream.flow written;
    Int32.to_int written

  let update_t stream n =
    let (Stream ({ parent = Parent parent; _ } as stream)) = stream in
    let tlast_p =
      match parent with
      | Connection { t_last; _ } ->
        t_last
      | Stream { t_last; _ } ->
        t_last
    in
    stream.t <- tlast_p + (n * 256 / stream.priority.weight)

  let mark_for_removal (Connection root) id closed =
    root.marked_for_removal <- (id, closed) :: root.marked_for_removal

  let implicitly_close_idle_stream descriptor max_seen_ids =
    let implicitly_close_stream descriptor =
      if Streamd.is_idle descriptor then
        (* From RFC7540§5.1.1:
         *   The first use of a new stream identifier implicitly closes all
         *   streams in the "idle" state that might have been initiated by
         *   that peer with a lower-valued stream identifier. *)
        Streamd.finish_stream descriptor Finished
    in
    let max_client_stream_id, max_pushed_stream_id = max_seen_ids in
    let stream_id = Streamd.id descriptor in
    if Stream_identifier.is_request stream_id then (
      if stream_id < max_client_stream_id then
        implicitly_close_stream descriptor)
    else if stream_id < max_pushed_stream_id then
      implicitly_close_stream descriptor

  (* Scheduling algorithm from https://goo.gl/3sSHXJ (based on nghttp2):
   *
   * 1  def schedule(p):
   * 2    if stream #p has data to send:
   * 3      send data for #p, update nsent[p]
   * 4      return
   * 5    if #p's queue is empty:
   * 6      return
   * 7    pop #i from queue
   * 8    update t_last[p] = t[i]
   * 9    schedule(i)
   * 10   if #i or its descendant is "active":
   * 11     update t[i] and push it into queue again
   * 12
   * 13 schedule(0)
   *)
  let flush t max_seen_ids =
    let rec schedule : type a. a node -> int * bool = function
      | Connection p ->
        (* The root can never send data. *)
        (match PriorityQueue.pop p.children with
        | Some ((id, (Stream i as i_node)), children') ->
          p.t_last <- i.t;
          let written, subtree_is_active = schedule i_node in
          if subtree_is_active then (
            update_t i_node written;
            p.children <- PriorityQueue.add id i_node children')
          else (
            implicitly_close_idle_stream i.descriptor max_seen_ids;
            (* XXX(anmonteiro): we may not want to remove from the tree right
             * away. *)
            p.children <- children');
          written, subtree_is_active
        | None ->
          (* Queue is empty, see line 6 above. *)
          0, false)
      | Stream ({ descriptor; _ } as p) as p_node ->
        if Streamd.requires_output descriptor then
          (* In this branch, flow-control has no bearing on activity, otherwise
           * a flow-controlled stream would be considered inactive (because it
           * can't make progress at the moment) and removed from the priority
           * tree altogether. *)
          let written = write t p_node in
          (* We check for activity again, because the stream may have gone
           * inactive after the call to `write` above. *)
          written, Streamd.requires_output descriptor
        else (
          match PriorityQueue.pop p.children with
          | Some ((id, (Stream i as i_node)), children') ->
            p.t_last <- i.t;
            let written, subtree_is_active = schedule i_node in
            if subtree_is_active then (
              update_t i_node written;
              p.children <- PriorityQueue.add id i_node children')
            else (
              implicitly_close_idle_stream i.descriptor max_seen_ids;
              p.children <- children');
            written, subtree_is_active
          | None ->
            (* Queue is empty, see line 6 above. *)
            0, false)
    in
    let (Connection root) = t in
    ignore (schedule t);
    root.marked_for_removal <-
      List.fold_left
        (fun acc (id, closed) ->
          (* When a stream completes, i.e. doesn't require more output and
           * enters the `Closed` state, we set a TTL value which represents
           * the * number of writer yields that the stream has before it is
           * removed * from the connection Hash Table. By doing this we avoid
           * losing some * potentially useful information regarding the
           * stream's state at the * cost of keeping it around for a little
           * while longer. *)
          if closed.Stream.ttl = 0 then (
            StreamsTbl.remove root.all_streams id;
            acc)
          else (
            closed.ttl <- closed.ttl - 1;
            (id, closed) :: acc))
        []
        root.marked_for_removal

  (* XXX(anmonteiro): Consider using `optint` for this?
   * https://github.com/mirage/optint
   *)
  let check_flow flow growth flow' =
    (* Check for overflow on 32-bit systems. *)
    Int32.compare flow' growth > 0 = (Int32.compare flow 0l > 0)
    && Int32.compare flow' Settings.WindowSize.max_window_size <= 0

  let add_flow : type a. a node -> int32 -> bool =
   fun t growth ->
    match t with
    | Connection ({ flow; _ } as root) ->
      let flow' = Int32.add flow growth in
      let valid_flow = check_flow flow growth flow' in
      if valid_flow then root.flow <- flow';
      valid_flow
    | Stream ({ flow; _ } as stream) ->
      let flow' = Int32.add flow growth in
      let valid_flow = check_flow flow growth flow' in
      if valid_flow then stream.flow <- flow';
      valid_flow

  let add_inflow : type a. a node -> int32 -> bool =
   fun t growth ->
    match t with
    | Connection ({ inflow; _ } as root) ->
      let inflow' = Int32.add inflow growth in
      let valid_inflow = check_flow inflow growth inflow' in
      if valid_inflow then root.inflow <- inflow';
      valid_inflow
    | Stream ({ inflow; _ } as stream) ->
      let inflow' = Int32.add inflow growth in
      let valid_inflow = check_flow inflow growth inflow' in
      if valid_inflow then stream.inflow <- inflow';
      valid_inflow

  let deduct_inflow : type a. a node -> int32 -> unit =
   fun t size ->
    match t with
    | Connection ({ inflow; _ } as root) ->
      (* no need to check, we verify that the peer is allowed to send. *)
      root.inflow <- Int32.sub inflow size
    | Stream ({ inflow; _ } as stream) ->
      stream.inflow <- Int32.sub inflow size

  let pp_hum fmt t =
    let rec pp_hum_inner level fmt t =
      let pp_binding fmt (i, Stream { children; t; _ }) =
        Format.fprintf
          fmt
          "\n%s%ld, %d -> [%a]"
          (String.make (level * 2) ' ')
          i
          t
          (pp_hum_inner (level + 1))
          children
      in
      PriorityQueue.pp pp_binding fmt t
    in
    pp_hum_inner 0 fmt t
end
