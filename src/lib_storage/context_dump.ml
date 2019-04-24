(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
(* Copyright (c) 2018 Nomadic Labs. <nomadic@tezcore.com>                    *)
(*                                                                           *)
(* Permission is hereby granted, free of charge, to any person obtaining a   *)
(* copy of this software and associated documentation files (the "Software"),*)
(* to deal in the Software without restriction, including without limitation *)
(* the rights to use, copy, modify, merge, publish, distribute, sublicense,  *)
(* and/or sell copies of the Software, and to permit persons to whom the     *)
(* Software is furnished to do so, subject to the following conditions:      *)
(*                                                                           *)
(* The above copyright notice and this permission notice shall be included   *)
(* in all copies or substantial portions of the Software.                    *)
(*                                                                           *)
(* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR*)
(* IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,  *)
(* FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL   *)
(* THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER*)
(* LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING   *)
(* FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER       *)
(* DEALINGS IN THE SOFTWARE.                                                 *)
(*                                                                           *)
(*****************************************************************************)

let version_name = "tezos-snapshot-1.0.0"

(*****************************************************************************)
module type Dump_interface = sig
  type index
  type context
  type tree
  type hash
  type step = string
  type key = step list
  type commit_info

  val commit_info_encoding : commit_info Data_encoding.t

  val hash_encoding : hash Data_encoding.t
  val blob_encoding : [ `Blob of MBytes.t ] Data_encoding.t
  val node_encoding : [ `Node of MBytes.t ] Data_encoding.t

  module Block_header : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val equal : t -> t -> bool
    val encoding : t Data_encoding.t
  end

  module Pruned_block : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val header : t -> Block_header.t
    val encoding : t Data_encoding.t
  end

  module Block_data : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val header : t -> Block_header.t
    val encoding : t Data_encoding.t
  end

  module Protocol_data : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val encoding : t Data_encoding.t
  end

  module Commit_hash : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t tzresult
    val encoding : t Data_encoding.t
  end

  (* hash manipulation *)
  val hash_export : hash -> [ `Node | `Blob ] * MBytes.t
  val hash_import : [ `Node | `Blob ]  -> MBytes.t -> hash tzresult
  val hash_equal : hash -> hash -> bool

  (* commit manipulation (for parents) *)
  val context_parents : context -> Commit_hash.t list Lwt.t

  (* Commit info *)
  val context_info : context -> commit_info
  val context_info_export : commit_info -> ( Int64.t * string * string )
  val context_info_import : ( Int64.t * string * string ) -> commit_info

  (* block header manipulation *)
  val get_context : index -> Block_header.t -> context option Lwt.t
  val set_context :
    info:commit_info -> parents:Commit_hash.t list -> context ->
    Block_header.t ->
    Block_header.t option Lwt.t

  (* for dumping *)
  val context_tree : context -> tree
  val tree_hash : context -> tree -> hash Lwt.t
  val sub_tree : tree -> key -> tree option Lwt.t
  val tree_list : tree -> ( step * [`Contents|`Node] ) list Lwt.t
  val tree_content : tree -> MBytes.t option Lwt.t

  (* for restoring *)
  val make_context : index -> context
  val update_context : context -> tree -> context
  val add_hash : index -> tree -> key -> hash -> tree option Lwt.t
  val add_mbytes : index -> tree -> key -> MBytes.t -> tree Lwt.t
  val add_dir : index -> tree -> key -> ( step * hash ) list -> tree option Lwt.t

end

module type S = sig
  type index
  type context
  type block_header
  type block_data
  type pruned_block
  type protocol_data

  val dump_contexts_fd :
    index ->
    (block_header * block_data *
     (block_header -> (pruned_block option * protocol_data option) tzresult Lwt.t)) list ->
    fd:Lwt_unix.file_descr -> unit tzresult Lwt.t
  val restore_contexts_fd : index -> fd:Lwt_unix.file_descr ->
    (block_header * block_data * pruned_block list * protocol_data list) list tzresult Lwt.t
end

type error += Writing_error of string
type error += Bad_read of string
type error += Bad_hash of string * MBytes.t * MBytes.t
type error += Context_not_found of MBytes.t



let () = register_error_kind `Permanent
    ~id:"context_dump.write.missing_space"
    ~title:"Cannot write in file for context dump"
    ~description:""
    ~pp:(fun ppf s ->
        Format.fprintf ppf
          "@[Unable@ to@ write@ for@ context@ dumping:@ %s@]"
          s
      )
    Data_encoding.(obj1 (req "context_dump_no_space" string) )
    (function Writing_error s -> Some s
            | _ -> None)
    (fun s -> Writing_error s)

let () = register_error_kind `Permanent
    ~id:"context_dump.write.context_not_found"
    ~title:"Cannot find context corresponding to hash"
    ~description:""
    ~pp:(fun ppf mb ->
        Format.fprintf ppf
          "@[No context with hash: %s@]"
          (MBytes.to_string mb))
    Data_encoding.(obj1 (req "context_dump_no_context" bytes) )
    (function Context_not_found mb -> Some mb
            | _ -> None)
    (fun mb -> Context_not_found mb)


let () = register_error_kind `Permanent
    ~id:"context_dump.read.bad_read"
    ~title:"Cannot read file"
    ~description:""
    ~pp:(fun ppf uerr ->
        Format.fprintf ppf
          "@[Error while reading file for context dumping: %s@]"
          uerr)
    Data_encoding.(obj1 (req "context_dump_cannot_read" string) )
    (function Bad_read e -> Some e
            | _ -> None)
    (fun e -> Bad_read e)

let () = register_error_kind `Permanent
    ~id:"context_dump.read.bad_hash"
    ~title:"Wrong hash given"
    ~description:""
    ~pp:(fun ppf ( ty, his, hshould ) ->
        Format.fprintf ppf
          "@[Wrong hash [%s] given: %s, should be %s@]"
          ty (MBytes.to_string his) (MBytes.to_string hshould))
    Data_encoding.( obj3
                      ( req "hash_ty" string )
                      ( req "hash_is" bytes )
                      ( req "hash_should" bytes ) )
    (function Bad_hash ( ty, his, hshould ) -> Some (ty, his, hshould )
            | _ -> None)
    (fun (ty, his, hshould) -> Bad_hash (ty, his,hshould))

module Make (I:Dump_interface) = struct

  type command =
    | Version of {
        version_name : string
      }
    | Root of {
        block_header: I.Block_header.t ;
        info: I.commit_info ;
        parents: I.Commit_hash.t list ;
        block_data : I.Block_data.t ;
      }
    | Node of {
        hash: [ `Node of MBytes.t ] ;
        path: I.key ;
        contents: (string * I.hash) list ;
      }
    | Blob of {
        hash: [ `Blob of MBytes.t ] ;
        path: I.key ;
        data: MBytes.t ;
      }
    | Proot of {
        pruned_block: I.Pruned_block.t
      }
    | Loot of {
        protocol_data: I.Protocol_data.t;
      }
    | End

  (* Command encoding. *)

  let blob_encoding =
    let open Data_encoding in
    case ~title:"blob" (Tag (Char.code 'b'))
      (obj3
         (req "hash" I.blob_encoding)
         (req "path" (list string))
         (req "data" bytes)
      )
      (function
        | Blob { hash ; path ; data} ->
            Some (hash, path, data)
        | _ -> None)
      (fun (hash, path, data) ->
         Blob { hash ; path ; data})

  let node_encoding =
    let open Data_encoding in
    let node_item_encoding =
      (obj2
         (req "name" string)
         (req "hash" I.hash_encoding)
      )
    in
    case ~title:"node" (Tag (Char.code 'd'))
      (obj3
         (req "hash" I.node_encoding)
         (req "path" (list string))
         (req "contents" (list node_item_encoding))
      )
      (function
        | Node { hash ; path ; contents} ->
            Some (hash, path, contents)
        | _ -> None)
      (fun (hash, path, contents) ->
         Node { hash ; path ; contents})

  let end_encoding =
    let open Data_encoding in
    case ~title:"end" (Tag (Char.code 'e'))
      empty
      (function End -> Some () | _ -> None)
      (fun () -> End)

  let loot_encoding =
    let open Data_encoding in
    case ~title:"loot" (Tag (Char.code 'l'))
      (obj1
         (req "proto_data" I.Protocol_data.encoding)
      )
      (function
        | Loot { protocol_data } -> Some protocol_data
        | _ -> None)
      (fun (protocol_data) ->
         Loot { protocol_data })

  let proot_encoding =
    let open Data_encoding in
    case ~title:"proot" (Tag (Char.code 'p'))
      (obj1 (req "pruned_block" I.Pruned_block.encoding))
      (function
        | Proot { pruned_block } ->
            Some pruned_block
        | _ -> None)
      (fun pruned_block ->
         Proot { pruned_block })

  let root_encoding =
    let open Data_encoding in
    case ~title:"root" (Tag (Char.code 'r'))
      (obj4
         (req "block_header" (dynamic_size I.Block_header.encoding))
         (req "info" I.commit_info_encoding)
         (req "parents" (list I.Commit_hash.encoding))
         (req "block_data" I.Block_data.encoding)
      )
      (function
        | Root { block_header ; info ; parents ; block_data } ->
            Some (block_header, info, parents, block_data)
        | _ -> None)
      (fun (block_header, info, parents, block_data) ->
         Root { block_header ; info ; parents ; block_data })

  let version_encoding =
    let open Data_encoding in
    case ~title:"version" (Tag (Char.code 'v'))
      (obj1
         (req "version" string))
      (function Version v -> Some v.version_name | _ -> None)
      (fun version_name -> Version {version_name})

  let command_encoding = Data_encoding.union [
      blob_encoding ;
      node_encoding ;
      end_encoding ;
      loot_encoding ;
      proot_encoding ;
      root_encoding ;
      version_encoding ;
    ]

  (* IO toolkit. *)

  let rec read_string rbuf ~len =
    let fd, buf, ofs, total = !rbuf in
    if Bytes.length buf - ofs < len then
      let blen = Bytes.length buf - ofs in
      let neu = Bytes.create (blen + 1_000_000) in
      Bytes.blit buf ofs neu 0 blen ;
      Lwt_unix.read fd neu blen 1_000_000 >>= fun bread ->
      total := !total + bread ;
      if bread = 0 then
        Lwt.fail End_of_file
      else
        let neu = if bread <> 1_000_000 then Bytes.sub neu 0 (blen + bread) else neu in
        rbuf := (fd, neu, 0, total) ;
        read_string rbuf ~len
    else
      let res = Bytes.sub_string buf ofs len in
      rbuf := (fd, buf, ofs + len, total) ;
      Lwt.return res

  let read_mbytes rbuf b =
    read_string rbuf ~len:(MBytes.length b) >>= fun string ->
    MBytes.blit_of_string string 0 b 0 (MBytes.length b) ;
    Lwt.return ()

  let set_int64 buf i =
    let b = Bytes.create 8 in
    EndianBytes.BigEndian.set_int64 b 0 i;
    Buffer.add_bytes buf b

  let get_int64 rbuf =
    read_string ~len:8 rbuf >>= fun s ->
    Lwt.return @@ EndianString.BigEndian.get_int64 s 0

  let set_mbytes buf b =
    set_int64 buf (Int64.of_int (MBytes.length b)) ;
    Buffer.add_bytes buf (MBytes.to_bytes b)

  let get_mbytes rbuf =
    get_int64 rbuf >|= Int64.to_int >>= fun l ->
    let b = MBytes.create l in
    read_mbytes rbuf b >>= fun () ->
    Lwt.return b

  (* Getter and setters *)

  let get_command rbuf =
    get_mbytes rbuf >|= fun bytes ->
    Data_encoding.Binary.of_bytes_exn command_encoding bytes

  let set_version buf =
    let bytes =
      Data_encoding.Binary.to_bytes_exn command_encoding (Version {version_name})
    in
    set_mbytes buf bytes

  let set_root buf block_header info parents block_data =
    let root = Root { block_header ; info ; parents ; block_data ; } in
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding root in
    set_mbytes buf bytes

  let set_node buf hash path contents =
    match I.hash_export hash with
    | `Blob, _ -> assert false
    | `Node, h ->
        let node = Node { hash = `Node h ; path ; contents ; } in
        let bytes = Data_encoding.Binary.to_bytes_exn command_encoding node in
        set_mbytes buf bytes

  let set_blob buf hash path data =
    match I.hash_export hash with
    | `Node, _ -> assert false
    | `Blob, h ->
        let blob = Blob { hash = `Blob h ; path ; data ; } in
        let bytes = Data_encoding.Binary.to_bytes_exn command_encoding blob in
        set_mbytes buf bytes

  let set_proot buf pruned_block =
    let proot = Proot { pruned_block ; } in
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding proot in
    set_mbytes buf bytes

  let set_loot buf protocol_data =
    let loot = Loot { protocol_data ; } in
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding loot in
    set_mbytes buf bytes

  let set_end buf =
    let bytes = Data_encoding.Binary.to_bytes_exn command_encoding End in
    set_mbytes buf bytes

  let dump_contexts_fd idx datas ~fd =
    (* Dumping *)
    let buf = Buffer.create 1_000_000 in
    let written = ref 0 in

    let flush () =
      let contents = Buffer.contents buf in
      Buffer.clear buf ;
      written := !written + String.length contents ;
      Lwt_utils_unix.write_string fd contents in

    let maybe_flush () =
      if Buffer.length buf > 1_000_000 then flush () else Lwt.return_unit in

    (* Noting the visited hashes *)
    let visited_hash = Hashtbl.create 1000 in
    let visited h = Hashtbl.mem visited_hash h in
    let set_visit h = Hashtbl.add visited_hash h () in

    (* Folding through a node *)
    let fold_tree_path ctxt path_rev tree =
      let cpt = ref 0 in
      let rec fold_tree_path ctxt path_rev tree =
        I.tree_list tree >>= fun keys ->
        let keys = List.sort (fun (a,_) (b,_) -> String.compare a b) keys in
        Lwt_list.map_s
          begin fun (name, kind) ->
            let path_rev = name :: path_rev in
            I.sub_tree tree [name] >>= function
            | None -> assert false
            | Some sub_tree ->
                I.tree_hash ctxt sub_tree >>= fun hash ->
                begin
                  if visited hash then Lwt.return_unit
                  else
                    begin
                      Tezos_stdlib.Utils.display_progress
                        ~refresh_rate:(!cpt, 1_000)
                        "Context: %dK elements, %dMiB written%!"
                        (!cpt / 1_000) (!written / 1_048_576) ;
                      incr cpt ;
                      set_visit hash; (* There cannot be a cycle *)
                      match kind with
                      | `Node ->
                          fold_tree_path ctxt path_rev sub_tree
                      | `Contents ->
                          begin I.tree_content sub_tree >>= function
                            | None ->
                                assert false
                            | Some data ->
                                set_blob buf hash path_rev data ;
                                maybe_flush ()
                          end
                    end
                end >>= fun () ->
                Lwt.return (name, hash)
          end
          keys >>= fun sub_keys ->
        I.tree_hash ctxt tree >>= fun hash ->
        set_node buf hash path_rev sub_keys;
        maybe_flush ()
      in
      fold_tree_path ctxt path_rev tree
    in
    Lwt.catch begin fun () ->
      set_version buf ;
      Error_monad.iter_s begin fun (bh, block_data, pruned_iterator) ->
        I.get_context idx bh >>= function
        | None ->
            fail @@ Context_not_found (I.Block_header.to_bytes bh)
        | Some ctxt ->
            let tree = I.context_tree ctxt in
            fold_tree_path ctxt [] tree >>= fun () ->
            Tezos_stdlib.Utils.display_progress_end ();
            I.context_parents ctxt >>= fun parents ->
            (* Dump pruned blocks *)
            let dump_pruned cpt pruned =
              Tezos_stdlib.Utils.display_progress
                ~refresh_rate:(cpt, 1_000)
                "History: %dK block, %dMiB written"
                (cpt / 1_000) (!written / 1_048_576) ;
              set_proot buf pruned;
              maybe_flush () in
            let rec aux cpt acc header =
              pruned_iterator header >>=? function
              | (None, None) -> return acc (* assert false *)
              | (None, Some protocol_data) ->
                  return (protocol_data :: acc)
              | (Some pred_pruned, Some protocol_data) ->
                  dump_pruned cpt pred_pruned >>= fun () ->
                  aux (succ cpt) (protocol_data :: acc)
                    (I.Pruned_block.header pred_pruned)
              | (Some pred_pruned, None) ->
                  dump_pruned cpt pred_pruned >>= fun () ->
                  aux (succ cpt) acc
                    (I.Pruned_block.header pred_pruned)
            in
            let starting_block_header = I.Block_data.header block_data in
            aux 0 [] starting_block_header >>=? fun protocol_datas ->
            (* Dump protocol data *)
            Lwt_list.iter_s (fun proto ->
                set_loot buf proto;
                maybe_flush () ;
              ) protocol_datas >>= fun () ->
            set_root buf bh (I.context_info ctxt) parents block_data;
            Tezos_stdlib.Utils.display_progress_end ();
            return_unit
      end datas >>=? fun () ->
      set_end buf;
      flush () >>= fun () ->
      return_unit
    end
      begin function
        | Unix.Unix_error (e,_,_) ->
            fail @@ Writing_error (Unix.error_message e)
        | Assert_failure (s,l,c) ->
            fail @@ Writing_error (Printf.sprintf "Assertion failed at %s %d %d" s l c)
        | err ->
            Error_monad.pp_exn Format.err_formatter err ;
            fail @@ Writing_error "Unknown error"
      end

  (* Restoring *)

  let restore_contexts_fd index ~fd =

    let read = ref 0 in
    let rbuf = ref (fd, Bytes.empty, 0, read) in

    let check_version v =
      if v <> version_name
      then fail @@ Bad_read "wrong version"
      else return ()
    in

    (* Check if a hash is right for you *)
    let check_hash his hshould =
      if I.hash_equal his hshould
      then return ()
      else fail @@ Bad_hash ("tree", snd @@ I.hash_export his, snd @@ I.hash_export hshould)
    in

    (* Editing the repository *)
    let add_blob ctxt path hash blob =
      I.add_mbytes index (I.context_tree ctxt) path blob >>= fun tree ->
      I.sub_tree tree path >>= function
      | None -> assert false
      | Some sub_tree -> begin
          I.tree_hash ctxt sub_tree >>= fun his ->
          check_hash his hash >>=? fun () ->
          return tree
        end
    in
    let add_dir ctxt hash path keys =
      I.add_dir index (I.context_tree ctxt) path keys >>= function
      | None -> fail @@ Bad_read "cannot add directory"
      | Some tree ->
          I.sub_tree tree path >>= function
          | None -> assert false
          | Some st ->
              I.tree_hash ctxt st >>= fun his ->
              check_hash his hash >>=? fun () ->
              return tree
    in

    let version_is_checked = ref false in

    let rec loop ctxt pruned_blocks protocol_datas acc cpt =
      Tezos_stdlib.Utils.display_progress
        ~refresh_rate:(cpt, 1_000)
        "Context: %dK elements, %dMiB read"
        (cpt / 1_000) (!read / 1_048_576) ;
      get_command rbuf >>= function
      | Root { block_header ; info ; parents ; block_data } ->
          begin I.set_context ~info ~parents ctxt block_header >>= function
            | None -> fail @@ Bad_read "context_hash does not correspond for block"
            | Some block_header ->
                let new_acc =
                  (block_header,
                   block_data,
                   List.rev pruned_blocks,
                   List.rev protocol_datas) :: acc
                in
                loop (I.make_context index) [] [] new_acc cpt
          end
      | Version { version_name } ->
          check_version version_name >>=? fun () ->
          if !version_is_checked then
            fail @@ Bad_read "numerous version names are providen"
          else begin
            version_is_checked := true ;
            loop ctxt pruned_blocks protocol_datas acc cpt
          end
      | Node { hash = `Node h ; path ; contents } ->
          Lwt.return (I.hash_import `Node h) >>=? fun hash ->
          add_dir ctxt hash path contents >>=? fun tree ->
          loop (I.update_context ctxt tree) pruned_blocks protocol_datas acc (succ cpt)
      | Blob { hash = `Blob h; path ; data } ->
          Lwt.return (I.hash_import `Blob h) >>=? fun hash ->
          add_blob ctxt path hash data >>=? fun tree ->
          loop (I.update_context ctxt tree) pruned_blocks protocol_datas acc (succ cpt)
      | Proot { pruned_block } ->
          loop ctxt
            (pruned_block :: pruned_blocks) protocol_datas
            acc (succ cpt)
      | Loot { protocol_data } ->
          loop ctxt
            pruned_blocks (protocol_data :: protocol_datas)
            acc (succ cpt)
      | End ->
          if not !version_is_checked then
            fail @@ Bad_read "snapshot version name is not provided"
          else begin
            if pruned_blocks <> [] || protocol_datas <> [] then
              fail (Bad_read "ill-formed snapshot: end mark not expected")
            else
              return (List.rev acc)
          end
    in
    Lwt.catch begin fun () ->
      loop (I.make_context index) [] [] [] 0
    end
      begin function
        | Unix.Unix_error (e,_,_) ->
            fail @@ Bad_read (Unix.error_message e)
        | Assert_failure (s,l,c) ->
            fail @@ Bad_read (Printf.sprintf "Bad assert at %s %d %d" s l c)
        | exc ->
            Format.kasprintf (fun x -> fail (Bad_read x))
              "unknown error: %a" Error_monad.pp_exn exc
      end
end
