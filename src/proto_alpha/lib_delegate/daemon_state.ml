open Proto_alpha
open Alpha_context

module Make(M:sig val name :string end)  : sig

  val get:
    #Client_context.wallet ->
    Signature.Public_key_hash.t ->
    Raw_level.t option tzresult Lwt.t

  val record:
    #Client_context.wallet ->
    Signature.Public_key_hash.t ->
    Raw_level.t ->
    unit tzresult Lwt.t

end = struct
  type t = (string * Raw_level.t) list

  let encoding : t Data_encoding.t =
    Data_encoding.assoc Raw_level.encoding

  let name = M.name

  let load (wallet : #Client_context.wallet) =
    wallet#load name encoding ~default:[]

  let save (wallet : #Client_context.wallet) list =
    wallet#write name list encoding

  let get (wallet : #Client_context.wallet) (delegate_key:Signature.public_key_hash) =
    wallet#with_lock
      (fun () ->
         load wallet >>=? fun l ->
         return (List.assoc_opt (Signature.Public_key_hash.to_short_b58check delegate_key) l)
      )

  let record (wallet : #Client_context.wallet) (delegate:Signature.public_key_hash) (new_lvl:Raw_level.t) =
    begin
      wallet#with_lock (fun () ->
          begin
            load wallet >>=? fun l  ->
            let delegate_key = Signature.Public_key_hash.to_short_b58check delegate
            in
            let remove_old l =
              List.filter
                (fun (_, lvl) -> Raw_level.diff new_lvl lvl < 50l (* FIXME: magic constant*))
                l
            in
            save wallet ((delegate_key, new_lvl)::
                         List.remove_assoc delegate_key (remove_old l))
          end)
    end
end


