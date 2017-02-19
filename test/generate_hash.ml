
open Base58
open Hash
open Lwt.Infix

type generator =
    Generator : {
      encoding: 'h encoding ;
      generator: string -> 'h list ;
    } -> generator

let generators = ref []

let register_generator
    (type a)
    (encoding : a encoding)
    (generator : string -> a list) =
  generators := Generator { encoding ; generator } :: !generators

let register (type t) (enc: t Base58.encoding) =
  register_generator enc
    (fun s ->
       match
         enc.of_raw
           (s ^
            Sodium.Random.Bytes.generate (enc.length - String.length s)) with
       | Some x -> [x]
       | None -> [])

let generate ?alphabet request =
  let rec find s = function
    | [] -> []
    | Generator { encoding ; generator } :: generators ->
        if not (Utils.has_prefix ~prefix:encoding.encoded_prefix s) then
          find s generators
        else
          let prefix =
            partial_decode ?alphabet request encoding.encoded_length in
          let len = String.length prefix in
          let ignored = String.length encoding.prefix in
          if len <= ignored then
            []
          else begin
            (* assert (String.sub prefix 0 ignored  = encoding.prefix) ; *)
            let msg = String.sub prefix ignored (len - ignored) in
            let msgs = generator msg in
            List.map
              (fun msg -> simple_encode encoding ?alphabet msg)
              msgs
          end in
  find request !generators


let () =
  register Hash.Block_hash.b58check_encoding ;
  register Hash.Protocol_hash.b58check_encoding ;
  if not (!Sys.interactive) then begin
    for i = 1 to Array.length Sys.argv - 1 do
      List.iter
        (Format.printf "%S@.")
        (generate Sys.argv.(i))
    done
  end
