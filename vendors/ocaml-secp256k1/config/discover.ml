let output_defines ppf symbols =
  let pp_sep ppf () = Format.pp_print_char ppf ' ' in
  let pp_symbol ppf sym =
    match sym with
    | None -> ()
    | Some (sym, None) -> Format.fprintf ppf "-D%s" sym
    | Some (sym, Some def) -> Format.fprintf ppf "-D%s=%s" sym def in
  let pp = Format.pp_print_list ~pp_sep pp_symbol in
  Format.fprintf ppf "(%a)%!" pp symbols

let hw = Config.hw_identifier ()
let sixtyfour = Sys.word_size = 64

let symbols = [
  (if sixtyfour then Some ("HAVE___INT128", None) else None) ;
  (if hw = "x86_64" then Some ("USE_ASM_X86_64", None) else None) ;
  Some ((if sixtyfour then "USE_SCALAR_4X64" else "USE_SCALAR_8X32"), None) ;
  Some ((if sixtyfour then "USE_FIELD_5X52" else "USE_FIELD_10X26"), None) ;
  Some ("USE_NUM_GMP", None) ;
  Some ("USE_SCALAR_INV_NUM", None) ;
  Some ("USE_FIELD_INV_NUM", None) ;
  Some ("SECP256K1_INLINE", Some "inline") ;
  Some ("SECP256K1_RESTRICT", Some "restrict") ;

  Some ("SECP256K1_TAG_PUBKEY_EVEN", Some "0x02") ;
  Some ("SECP256K1_TAG_PUBKEY_ODD", Some "0x03") ;
  Some ("SECP256K1_TAG_PUBKEY_UNCOMPRESSED", Some "0x04") ;
  Some ("SECP256K1_TAG_PUBKEY_HYBRID_EVEN", Some "0x06") ;
  Some ("SECP256K1_TAG_PUBKEY_HYBRID_ODD", Some "0x07") ;

  Some ("ENABLE_MODULE_RECOVERY", None) ;
]

let () =
  let oc = open_out "c_flags.sexp" in
  let ppf = Format.formatter_of_out_channel oc in
  output_defines ppf symbols ;
  close_out oc
