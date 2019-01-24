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

  module Block_data : sig
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
  val add_mbytes : tree -> key -> MBytes.t -> tree Lwt.t
  val add_dir : index -> tree -> key -> ( step * hash ) list -> tree option Lwt.t

end

module type S = sig
  type index
  type context
  type block_header
  type block_data

  val dump_contexts_fd : index -> (block_header * block_data) list -> fd:Lwt_unix.file_descr -> unit tzresult Lwt.t
  val restore_contexts_fd : index -> fd:Lwt_unix.file_descr ->
    (block_header * block_data) list tzresult Lwt.t
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

 * Root: 'r' (block_header : mbytes, date : int, author : string, message : string, parents : mbytes list)
   Root marks a context as complete, everything happening after the ROOT
   instruction will be in a different context, and no other modification will be
   performed on that context.
   block_header must contain the hash of the corresponding context.
   date, author and message must be the ones used by Tezos.
   parents must be the list of ancestors of the context (which would have a single element).
   parents should be ordered by hash decreasing (though if there are several parents, something really odd happened).

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
   Root
   End

   In case several trees are given, they should be exported in the order given as input.

*)


module Make (I:Dump_interface)
= struct

  let context_dump_magic = "V1Tezos-Context-dump"

  type command_type = Root | Directory | Blob | End | Meta

  let set_command fd c =
    Lwt_utils_unix.write_string fd
      ( match c with
        | Root -> "r" | Directory -> "d" | Blob -> "b" | End -> "e" | Meta -> "m")
  let get_command fd =
    Lwt.try_bind
      (fun () -> Lwt_utils_unix.read_string fd ~len:1)
      begin function
        | "r" -> return Root
        | "d" -> return Directory
        | "b" -> return Blob
        | "e" -> return End
        | "m" -> return Meta
        | opcode -> fail @@ Bad_read ("wrong opcode: " ^ opcode)
      end
      begin function
        | End_of_file -> fail @@ Bad_read ("Unexpected end of file")
        | _ -> fail @@ Bad_read ("Unknown read error")
      end

  let set_hash_type fd ty = set_command fd
      ( match ty with
        | `Node -> Directory
        | `Blob -> Blob )
  let get_hash_type fd =
    get_command fd >>=? function
    | Root -> fail @@ Bad_read ("can't reference commit here")
    | Directory -> return `Node
    | Blob -> return `Blob
    | End -> fail @@ Bad_read ("unexpected end of file")
    | Meta -> fail @@ Bad_read ("can't reference meta here")


  let set_int64 fd i =
    let b = Bytes.create 8 in
    EndianBytes.BigEndian.set_int64 b 0 i;
    Lwt_utils_unix.write_bytes fd b
  let get_int64 fd =
    Lwt_utils_unix.read_string ~len:8 fd >>= fun s ->
    Lwt.return @@ EndianString.BigEndian.get_int64 s 0
  let set_int fd i =
    set_int64 fd @@ Int64.of_int i
  let get_int fd =
    get_int64 fd >|= Int64.to_int

  let set_mbytes fd b =
    set_int fd @@ MBytes.length b >>= fun () ->
    Lwt_utils_unix.write_mbytes fd b
  let get_mbytes fd =
    get_int fd >>= fun l ->
    let b = MBytes.create l in
    Lwt_utils_unix.read_mbytes fd b >>= fun () ->
    Lwt.return b

  let set_hash fd h =
    let (ty,mb) = I.hash_export h in
    set_hash_type fd ty >>= fun () ->
    set_mbytes fd mb
  let get_hash fd =
    get_hash_type fd >>=? fun ty ->
    get_mbytes fd >>= fun mb ->
    Lwt.return @@ I.hash_import ty mb

  let set_string fd s =
    set_int fd @@ String.length s >>= fun () ->
    Lwt_utils_unix.write_string fd s
  let get_string fd =
    get_int fd >>= fun l ->
    Lwt_utils_unix.read_string ~len:l fd

  let set_list fd f l =
    set_int fd (List.length l) >>= fun () ->
    Lwt_list.iter_s (f fd) l
  let get_rev_list fd f =
    get_int fd >>= fun l ->
    if l < 0
    then fail @@ Bad_read "negative list length"
    else
      let rec loop acc n =
        if n = 0 then return acc
        else f fd >>=? fun x -> loop (x::acc) (pred n)
      in loop [] l

  let set_keys_hashs fd keys =
    set_list fd
      (fun fd (k,h) -> set_string fd k >>= fun () -> set_hash fd h)
      keys
  let get_keys_hashs fd =
    get_rev_list fd begin fun fd ->
      get_string fd >>= fun k ->
      get_hash fd >>=? fun h ->
      return (k,h)
    end

  let set_rev_path fd path_rev = set_list fd set_string path_rev
  let get_path fd =
    get_rev_list fd (fun fd -> get_string fd >>= return)

  let set_blob fd hash path_rev blob =
    set_command fd Blob >>= fun () ->
    match I.hash_export hash with
    | `Blob, mbhash ->
        begin
          set_mbytes fd mbhash >>= fun () ->
          set_rev_path fd path_rev >>= fun () ->
          set_mbytes fd blob
        end
    | `Node, _ -> assert false
  let get_blob fd =
    get_mbytes fd >>= fun mbhash ->
    Lwt.return @@ I.hash_import `Blob mbhash >>=? fun hash ->
    get_path fd >>=? fun path ->
    get_mbytes fd >>= fun blob ->
    return ( hash, path, blob )

  let set_dir fd hash path_rev keys =
    set_command fd Directory >>= fun () ->
    match I.hash_export hash with
    | `Node, mbhash ->
        begin
          set_mbytes fd mbhash >>= fun () ->
          set_rev_path fd path_rev >>= fun () ->
          set_keys_hashs fd keys
        end
    | `Blob, _ -> assert false
  let get_dir fd =
    get_mbytes fd >>= fun mbhash ->
    Lwt.return @@ I.hash_import `Node mbhash >>=? fun hash ->
    get_path fd >>=? fun path ->
    get_keys_hashs fd >>=? fun keys ->
    return ( hash, path, keys )

  let set_root fd bh info parents meta_h =
    set_command fd Root >>= fun () ->
    let mbhash = I.Block_header.to_bytes bh in
    set_mbytes fd mbhash >>= fun () ->
    let ( date, author, message ) = I.context_info_export info in
    set_int64 fd date >>= fun () ->
    set_string fd author >>= fun () ->
    set_string fd message >>= fun () ->
    set_list fd (fun fd ch -> set_mbytes fd @@ I.Commit_hash.to_bytes ch) parents >>= fun () ->
    set_mbytes fd (Context_hash.to_bytes meta_h)
  let get_root fd meta_tbl =
    get_mbytes fd >>= fun mbhash ->
    match I.Block_header.of_bytes mbhash with
    | None -> fail @@ Bad_read "wrong block header"
    | Some bh ->
        get_int64 fd >>= fun date ->
        get_string fd >>= fun author ->
        get_string fd >>= fun message ->
        let info = I.context_info_import ( date, author, message ) in
        get_rev_list fd
          (fun fd -> get_mbytes fd >>= fun mbh ->
            Lwt.return @@ I.Commit_hash.of_bytes mbh)
        >>=? fun parents ->
        get_mbytes fd >>= fun h ->
        Lwt.return @@ Context_hash.of_bytes h >>=? fun h ->
        begin match Hashtbl.find_opt meta_tbl h with
          | None -> fail @@ Bad_read "unknown meta hash"
          | Some meta -> return meta
        end  >>=? fun meta ->
        return ( bh, info, List.rev parents, meta)

  let set_meta fd tbl b =
    let h = Context_hash.hash_bytes [b] in
    if Hashtbl.mem tbl h
    then Lwt.return h
    else
      begin
        Hashtbl.add tbl h ();
        set_command fd Meta >>= fun () ->
        set_mbytes fd (Context_hash.to_bytes h) >>= fun () ->
        set_mbytes fd b >>= fun () ->
        Lwt.return h
      end
  let get_meta fd tbl =
    get_mbytes fd >>= fun h ->
    Lwt.return @@ Context_hash.of_bytes h >>=? fun h ->
    get_mbytes fd >>= fun b ->
    let h' = Context_hash.hash_bytes [b] in
    if Context_hash.equal h h'
    then ( Hashtbl.add tbl h b; return () )
    else fail @@ Bad_read "wrong meta hash"


  (* Dumping *)

  let dump_contexts_fd idx blocks ~fd =

    (* Setting the magic *)
    let magic_set () =
      Lwt_utils_unix.write_string fd context_dump_magic
    in

    (* Noting the visited hashes *)
    let visited_hash = Hashtbl.create 1000 in
    let visited h = Hashtbl.mem visited_hash h in
    let set_visit h = Hashtbl.add visited_hash h () in

    (* Folding through a node *)
    let rec fold_tree_path ctxt path_rev tree =
      I.tree_list tree >>= fun keys ->
      let keys = List.sort (fun (a,_) (b,_) -> String.compare a b) keys in
      Lwt_list.fold_left_s
        begin fun acc (name,kind) ->
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
                    set_visit hash; (* There cannot be a cycle *)
                    match kind with
                    | `Contents ->
                        begin
                          I.tree_content sub_tree >>= function
                          | None -> assert false
                          | Some data -> set_blob fd hash path_rev data
                        end
                    | `Node -> fold_tree_path ctxt path_rev sub_tree
                  end
              end >>= fun () ->
              Lwt.return ( (name,hash)::acc )
        end
        [] keys >>= fun sub_keys_rev ->
      I.tree_hash ctxt tree >>= fun hash ->
      set_dir fd hash path_rev (List.rev sub_keys_rev)
    in

    (* Meta table initialisation *)
    let meta_tbl : (Context_hash.t,unit) Hashtbl.t = Hashtbl.create 127 in

    (* Execution *)
    Lwt.catch begin fun () ->
      magic_set () >>= fun () ->
      Error_monad.iter_s
        (fun (bh,meta) ->
           I.get_context idx bh >>= function
           | None -> fail @@ Context_not_found (I.Block_header.to_bytes bh)
           | Some ctxt ->
               let tree = I.context_tree ctxt in
               fold_tree_path ctxt [] tree >>= fun () ->
               I.context_parents ctxt >>= fun parents ->
               set_meta fd meta_tbl (I.Block_data.to_bytes meta)
               >>= fun meta_h ->
               (* Lwt_list.map_s (set_meta fd meta_tbl) metas *)
               set_root fd bh (I.context_info ctxt) parents meta_h
               >>= return
        )
        blocks
      >>=? fun () ->
      set_command fd End >>= return
    end
      begin function
        | Unix.Unix_error (e,_,_) -> fail @@ Writing_error (Unix.error_message e)
        | Assert_failure (s,l,c) ->
            fail @@ Writing_error (Printf.sprintf "Bad assert at %s %d %d" s l c)
        | err ->
            Error_monad.pp_exn Format.err_formatter err ;
            fail @@ Writing_error "unknown error"
      end

  (* Restoring *)

  let restore_contexts_fd index ~fd =

    (* Magic check *)
    let magic_check () =
      let magic_length = String.length context_dump_magic in
      Lwt_utils_unix.read_string ~len:magic_length fd >>= fun magic ->
      if magic <> context_dump_magic
      then fail @@ Bad_read "wrong magic"
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
      I.add_mbytes (I.context_tree ctxt) path blob >>= fun tree ->
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

    (* Meta table declaration *)
    let meta_tbl : ( Context_hash.t, MBytes.t ) Hashtbl.t = Hashtbl.create 127 in

    (* The big loop *)
    let rec loop ctxt acc =
      get_command fd >>=?
      function
      | Root ->
          begin
            get_root fd meta_tbl >>=? fun ( bh_should, info, parents, meta) ->
            I.set_context ~info ~parents ctxt bh_should >>= function
            | None -> fail @@ Bad_read "context_hash does not correspond for block"
            | Some bh ->
                begin
                  match I.Block_data.of_bytes meta with
                  | Some md -> return md
                  | None -> fail @@ Bad_read "wrong meta data"
                end >>=? fun meta ->
                loop
                  (I.make_context index)
                  (( bh, meta ) :: acc)
          end
      | Directory ->
          get_dir fd >>=?
          fun ( hash, path, keys ) -> add_dir ctxt hash path keys >>=?
          fun tree -> loop (I.update_context ctxt tree) acc
      | Blob ->
          get_blob fd >>=?
          fun ( hash, path, blob ) -> add_blob ctxt path hash blob >>=?
          fun tree -> loop (I.update_context ctxt tree) acc
      | End -> return acc
      | Meta -> get_meta fd meta_tbl >>=? fun () -> loop ctxt acc
    in

    (* Execution *)
    Lwt.catch (fun () ->
        magic_check () >>=? fun () ->
        loop (I.make_context index) []
      )
      (function
        | Unix.Unix_error (e,_,_) -> fail @@ Bad_read (Unix.error_message e)
        | Assert_failure (s,l,c) ->
            fail @@ Bad_read (Printf.sprintf "Bad assert at %s %d %d" s l c)
        | exc ->
            let msg = Printf.sprintf "unknown error: %s" (Printexc.to_string exc) in
            fail (Bad_read msg)
      )

    >>=? fun l ->
    return @@ List.rev l

end
