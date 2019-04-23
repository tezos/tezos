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

module type Dump_interface = sig
  type index
  type context
  type tree
  type hash
  type step = string
  type key = step list
  type commit_info

  module Block_header : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val equal : t -> t -> bool
  end

  module Pruned_block : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
    val header : t -> Block_header.t
  end

  module Block_data : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
  end

  module Protocol_data : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t option
  end

  module Commit_hash : sig
    type t
    val to_bytes : t -> MBytes.t
    val of_bytes : MBytes.t -> t tzresult
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
     (block_header -> (pruned_block option * protocol_data option) tzresult Lwt.t) * block_header) list ->
    fd:Lwt_unix.file_descr -> unit tzresult Lwt.t
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

(* The context dumping file format

   ==Overview

   The dumping file format aims at providing an efficient and simple format to
   input and output contexts.

   It supports the dumping of several contexts with simplification of sharing
   through the hashes provided by the context.

   The file must start with the ASCII sentence "V1Tezos-Context-dump" (quotes not
   included).

   Instructions given as "must" have to be respected to be valid. Instructions
   given as "should" are required for the file to be in the "normal form" that will
   ensure its unicity for a given context, they are however not checked by the
   import functionnality.

   ==Data encoding

   Several types are used in this format. Note that at any point in the file, the
   type of the next piece of data is known. Here is how the different pieces of
   data are encoded.

 * Integers are stored as big-endian 64-bit integers
   => usage of EndianBytes.BigEndian.{set,get}_int64 is advised
 * Strings, bytes and MBytes are stored as an integer indicating of the length of
   the data (in bytes), followed by the data itself
 * Lists are stored as an integer indicating the length of the list (in number of
   elements), followed by each element starting with the head.
 * Tuples are stored as each element subsequently.
 * Hashes (which should be seen as MBytes) are Blake2b hashes.

   ==The current context

   This file format is tightly linked to the notion of context
   (see src/lib_storage/context.mli).

   It is to be noted that some data may be shared between context, and not
   necessarily under the same path. But, we'd rather build our contexts
   incrementally, without having to use a temporary, fake index.

   So, when the file starts (whether for dumping or restoring). Assume there is a
   current context. Any path given in the file will be respective to that current
   context. Note that the current context changes after a Root instruction.

   If an element as already been inserted under a different path, it should not be reinserted.

   ==Storage instructions

   There are three storage instructions, they start with a 1-byte opcode that
   indicate the instruction, followed by the relevant data.

 * Blob: 'b' (hash : mbytes, path_rev : string list, data : mbytes)
   Blob adds the data under the right path in the current context.
   hash must match data.
   path_rev must be given reversed.

 * Dir: 'd' (hash : mbytes, path_rev : string list, keys : (key:string,khash:mbytes) list)
   Dir adds a directory under the right path in the current context.
   Each hash must correspond to a blob or directory already present in the index.
   hash must match the complete directory.
   path_rev must be given reversed.
   keys should be ordered by String.compare on its first element.

 * Root: 'r' (block_data : mbytes, date : int, author : string, message : string, parents : mbytes list)
   Root marks a context as complete, everything happening after the ROOT
   instruction will be in a different context, and no other modification will be
   performed on that context.
   block_header must contain the hash of the corresponding context.
   date, author and message must be the ones used by Tezos.
   parents must be the list of ancestors of the context (which would have a single element).
   parents should be ordered by hash decreasing (though if there are several parents, something really odd happened).

 * Proot: 'p' (pruned_block_data: mbytes)
   Adds a history block (only the chain data without context).
   For a snapshot to be valid, there should be Proot entries for at least a number of blocks
   preceding each Root according to the Root's operation ttl.

 * Loot: 'l' (protocol_data: mbytes)
   Adds the protocol data to the snapshots. It contains a pair (level * data) for each
   transition blocks of the exported block list. For a snapshot to be valid, there should be
   Loot entries.

 * End: 'e'
   This must be the end of the file.

   ==Storage order

   A Dir or Root instruction must be present after any value in it has already been
   inserted. Dir elements should be inserted in the order provided by String.compare.

   example:

   a tree of the form:

    root
   /   \
   a     b
   / \    |
   aa ab   ba

   Will be exported as:

   Blob root/a/aa
   Blob root/a/ab
   Dir root/a
   Blob root/b/ba
   Blob root/b
   Dir root
   Proot data
   Proot data
   Root
   ...
   End

   In case several trees are given, they should be exported in the order given as input.

*)


module Make (I:Dump_interface)
= struct

  let context_dump_magic = "V1Tezos-Context-dump"

  type command_type = Root | Directory | Blob | Meta | Proot | Loot | End

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

  let set_command buf c =
    Buffer.add_string buf
      ( match c with
        | Root -> "r"
        | Directory -> "d"
        | Blob -> "b"
        | Meta -> "m"
        | Proot -> "p"
        | Loot -> "l"
        | End -> "e" )
  let get_command rbuf =
    Lwt.try_bind
      (fun () -> read_string rbuf ~len:1)
      begin function
        | "r" -> return Root
        | "d" -> return Directory
        | "b" -> return Blob
        | "m" -> return Meta
        | "p" -> return Proot
        | "l" -> return Loot
        | "e" -> return End
        | opcode -> fail @@ Bad_read ("wrong opcode: " ^ opcode)
      end
      begin function
        | End_of_file -> fail @@ Bad_read ("Unexpected end of file")
        | _ -> fail @@ Bad_read ("Unknown read error")
      end

  let set_hash_type buf ty = set_command buf
      ( match ty with
        | `Node -> Directory
        | `Blob -> Blob )
  let get_hash_type rbuf =
    get_command rbuf >>=? function
    | Root -> fail @@ Bad_read ("can't reference commit here")
    | Directory -> return `Node
    | Blob -> return `Blob
    | Meta -> fail @@ Bad_read ("can't reference meta here")
    | Proot -> fail @@ Bad_read ("can't have a pruned block here")
    | Loot -> fail @@ Bad_read ("can't have protocol data here")
    | End -> fail @@ Bad_read ("unexpected end of file")

  let set_int64 buf i =
    let b = Bytes.create 8 in
    EndianBytes.BigEndian.set_int64 b 0 i;
    Buffer.add_bytes buf b
  let get_int64 rbuf =
    read_string ~len:8 rbuf >>= fun s ->
    Lwt.return @@ EndianString.BigEndian.get_int64 s 0
  let set_int buf i =
    set_int64 buf @@ Int64.of_int i
  let get_int rbuf =
    get_int64 rbuf >|= Int64.to_int

  let set_mbytes buf b =
    set_int buf (MBytes.length b) ;
    Buffer.add_bytes buf (MBytes.to_bytes b)
  let get_mbytes rbuf =
    get_int rbuf >>= fun l ->
    let b = MBytes.create l in
    read_mbytes rbuf b >>= fun () ->
    Lwt.return b

  let set_hash buf h =
    let (ty,mb) = I.hash_export h in
    set_hash_type buf ty ;
    set_mbytes buf mb
  let get_hash rbuf =
    get_hash_type rbuf >>=? fun ty ->
    get_mbytes rbuf >>= fun mb ->
    Lwt.return @@ I.hash_import ty mb

  let set_string buf s =
    set_int buf (String.length s) ;
    Buffer.add_string buf s
  let get_string rbuf =
    get_int rbuf >>= fun l ->
    read_string ~len:l rbuf

  let set_list buf f l =
    set_int buf (List.length l) ;
    List.iter (f buf) l
  let get_rev_list rbuf f =
    get_int rbuf >>= fun l ->
    if l < 0
    then fail @@ Bad_read "negative list length"
    else
      let rec loop acc n =
        if n = 0 then return acc
        else f rbuf >>=? fun x -> loop (x::acc) (pred n)
      in loop [] l

  let set_keys_hashs buf keys =
    set_list buf
      (fun buf (k,h) -> set_string buf k ; set_hash buf h)
      keys
  let get_keys_hashs rbuf =
    get_rev_list rbuf begin fun rbuf ->
      get_string rbuf >>= fun k ->
      get_hash rbuf >>=? fun h ->
      return (k,h)
    end

  let set_rev_path buf path_rev = set_list buf set_string path_rev
  let get_path rbuf =
    get_rev_list rbuf (fun rbuf -> get_string rbuf >>= return)

  let set_blob buf hash path_rev blob =
    set_command buf Blob ;
    match I.hash_export hash with
    | `Blob, mbhash ->
        begin
          set_mbytes buf mbhash ;
          set_rev_path buf path_rev ;
          set_mbytes buf blob
        end
    | `Node, _ -> assert false
  let get_blob rbuf =
    get_mbytes rbuf >>= fun mbhash ->
    Lwt.return @@ I.hash_import `Blob mbhash >>=? fun hash ->
    get_path rbuf >>=? fun path ->
    get_mbytes rbuf >>= fun blob ->
    return ( hash, path, blob )

  let set_dir buf hash path_rev keys =
    set_command buf Directory ;
    match I.hash_export hash with
    | `Node, mbhash ->
        begin
          set_mbytes buf mbhash ;
          set_rev_path buf path_rev ;
          set_keys_hashs buf keys
        end
    | `Blob, _ -> assert false
  let get_dir rbuf =
    get_mbytes rbuf >>= fun mbhash ->
    Lwt.return @@ I.hash_import `Node mbhash >>=? fun hash ->
    get_path rbuf >>=? fun path ->
    get_keys_hashs rbuf >>=? fun keys ->
    return ( hash, path, keys )

  let set_root buf bh info parents meta_h =
    set_command buf Root ;
    let mbhash = I.Block_header.to_bytes bh in
    set_mbytes buf mbhash ;
    let ( date, author, message ) = I.context_info_export info in
    set_int64 buf date ;
    set_string buf author ;
    set_string buf message ;
    set_list buf (fun buf ch -> set_mbytes buf @@ I.Commit_hash.to_bytes ch) parents ;
    set_mbytes buf (Context_hash.to_bytes meta_h)
  let get_root rbuf meta_tbl =
    get_mbytes rbuf >>= fun mbhash ->
    match I.Block_header.of_bytes mbhash with
    | None -> fail @@ Bad_read "wrong block header"
    | Some bh ->
        get_int64 rbuf >>= fun date ->
        get_string rbuf >>= fun author ->
        get_string rbuf >>= fun message ->
        let info = I.context_info_import ( date, author, message ) in
        get_rev_list rbuf
          (fun rbuf -> get_mbytes rbuf >>= fun mbh ->
            Lwt.return @@ I.Commit_hash.of_bytes mbh)
        >>=? fun parents ->
        get_mbytes rbuf >>= fun h ->
        Lwt.return @@ Context_hash.of_bytes h >>=? fun h ->
        begin match Hashtbl.find_opt meta_tbl h with
          | None -> fail @@ Bad_read "unknown meta hash"
          | Some meta -> return meta
        end  >>=? fun meta ->
        return ( bh, info, List.rev parents, meta)

  let set_meta buf tbl b =
    let h = Context_hash.hash_bytes [b] in
    if Hashtbl.mem tbl h
    then Lwt.return h
    else
      begin
        Hashtbl.add tbl h ();
        set_command buf Meta ;
        set_mbytes buf (Context_hash.to_bytes h) ;
        set_mbytes buf b ;
        Lwt.return h
      end
  let get_meta rbuf tbl =
    get_mbytes rbuf >>= fun h ->
    Lwt.return @@ Context_hash.of_bytes h >>=? fun h ->
    get_mbytes rbuf >>= fun b ->
    let h' = Context_hash.hash_bytes [b] in
    if Context_hash.equal h h'
    then ( Hashtbl.add tbl h b; return () )
    else fail @@ Bad_read "wrong meta hash"


  (* Dumping *)

  let dump_contexts_fd idx datas ~fd =

    let buf = Buffer.create 1_000_000 in
    let written = ref 0 in
    let flush () =
      let contents = Buffer.contents buf in
      Buffer.clear buf ;
      written := !written + String.length contents ;
      Lwt_utils_unix.write_string fd contents in
    let maybe_flush () =
      if Buffer.length buf > 1_000_000 then flush () else Lwt.return_unit in
    (* Setting the magic *)
    let magic_set () =
      Lwt_utils_unix.write_string fd context_dump_magic
    in

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
          begin fun (name,kind) ->
            let path_rev = name :: path_rev in
            I.sub_tree tree [name] >>= function
            | None -> assert false
            | Some sub_tree ->
                I.tree_hash ctxt sub_tree >>= fun hash ->
                begin
                  if visited hash
                  then Lwt.return_unit
                  else
                    begin
                      Tezos_stdlib.Utils.display_progress
                        ~refresh_rate:(!cpt, 1_000)
                        "Context: %dK elements, %dMiB written%!"
                        (!cpt / 1_000) (!written / 1_048_576) ;
                      incr cpt ;
                      set_visit hash; (* There cannot be a cycle *)
                      match kind with
                      | `Contents ->
                          begin
                            I.tree_content sub_tree >>= function
                            | None -> assert false
                            | Some data ->
                                set_blob buf hash path_rev data ;
                                maybe_flush ()
                          end
                      | `Node -> fold_tree_path ctxt path_rev sub_tree
                    end
                end >>= fun () ->
                Lwt.return ( (name,hash) )
          end
          keys >>= fun sub_keys ->
        I.tree_hash ctxt tree >>= fun hash ->
        set_dir buf hash path_rev (sub_keys) ;
        maybe_flush ()
      in
      fold_tree_path ctxt path_rev tree
    in

    (* Meta table initialisation *)
    let meta_tbl : (Context_hash.t,unit) Hashtbl.t = Hashtbl.create 127 in

    (* Execution *)
    Lwt.catch begin fun () ->
      magic_set () >>= fun () ->
      Error_monad.iter_s
        (fun (bh, meta, pruned_iterator, starting_block_header) ->
           I.get_context idx bh >>= function
           | None -> fail @@ Context_not_found (I.Block_header.to_bytes bh)
           | Some ctxt ->
               let tree = I.context_tree ctxt in
               fold_tree_path ctxt [] tree >>= fun () ->
               Tezos_stdlib.Utils.display_progress_end ();
               I.context_parents ctxt >>= fun parents ->
               set_meta buf meta_tbl (I.Block_data.to_bytes meta) >>= fun meta_h ->
               (* Dump pruned blocks *)
               let dump_pruned cpt pruned =
                 Tezos_stdlib.Utils.display_progress
                   ~refresh_rate:(cpt, 1_000)
                   "History: %dK block, %dMiB written"
                   (cpt / 1_000) (!written / 1_048_576) ;
                 set_command buf Proot ;
                 let mbhash = I.Pruned_block.to_bytes pruned in
                 set_mbytes buf mbhash ;
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
               in aux 0 [] starting_block_header >>=? fun protocol_datas ->
               (* Dump protocol data *)
               Lwt_list.iter_s (fun proto ->
                   set_command buf Loot ;
                   let mbhash = I.Protocol_data.to_bytes proto in
                   set_mbytes buf mbhash ;
                   maybe_flush () ;
                 ) protocol_datas >>= fun () ->
               set_root buf bh (I.context_info ctxt) parents meta_h ;
               return_unit
        )
        datas
      >>=? fun () ->
      set_command buf End;
      flush () >>= fun () ->
      Tezos_stdlib.Utils.display_progress_end ();
      return_unit
    end
      begin function
        | Unix.Unix_error (e,_,_) -> fail @@ Writing_error (Unix.error_message e)
        | Assert_failure (s,l,c) ->
            fail @@ Writing_error (Printf.sprintf "Bad assert at %s %d %d" s l c)
        | err ->
            Error_monad.pp_exn Format.err_formatter err ;
            fail @@ Writing_error "unknown error"
      end
end
