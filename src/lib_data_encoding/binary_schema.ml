(**************************************************************************)
(*                                                                        *)
(*    Copyright (c) 2014 - 2018.                                          *)
(*    Dynamic Ledger Solutions, Inc. <contact@tezos.com>                  *)
(*                                                                        *)
(*    All rights reserved. No warranty, explicit or implicit, provided.   *)
(*                                                                        *)
(**************************************************************************)

open Encoding

type integer_extended = [ Binary_size.integer | `Int32 | `Int64 ]

type field_descr =
  | Named_field of string * Kind.t * layout
  | Anonymous_field of Kind.t * layout
  | Dynamic_field of int
  | Option_indicator_field of string

and layout =
  | Zero_width
  | Int of integer_extended
  | Bool
  | RangedInt of int * int
  | RangedFloat of float * float
  | Float
  | Bytes
  | String
  | Enum of Binary_size.integer * string
  | Seq of layout (* For arrays and lists *)
  | Ref of string

and fields = field_descr list

and toplevel_encoding =
  | Obj of { fields : fields }
  | Cases of { kind : Kind.t ;
               tag_size : Binary_size.tag_size ;
               cases : (int * string option * fields) list }
  | Int_enum of { size : Binary_size.integer ;
                  cases : (int * string) list }

and description =
  { name : string ;
    description : string option }

type t = (description * toplevel_encoding) list

module Printer = struct

  type table =
    { title : string ;
      description : string option ;
      headers : string list ;
      body : string list list }

  type print_structure =
    | Table of table
    | Union of string * string option * Binary_size.tag_size * table list

  let pp_kind ppf = function
    | `Fixed size -> Format.fprintf ppf  "%d byte%s" size (if size = 1 then "" else "s")
    | `Variable -> Format.fprintf ppf "Variable size"
    | `Dynamic -> Format.fprintf ppf "Determined from data"

  let pp_int ppf (int : integer_extended) =
    Format.fprintf ppf "%s"
      begin
        match int with
        | `Int16 -> "16 bit Signed Integer"
        | `Int31 -> "32 bit Signed Integer in the range [2^30, 2^30-1]"
        | `Uint30 -> "32 bit Signed Integer in the range [0, 2^30-1]"
        | `Int32 -> "32 bit Signed Integer"
        | `Int64 -> "64 bit Signed Integer"
        | `Int8 -> "8 bit Signed Integer"
        | `Uint16 -> "16 bit Unsigned Integer"
        | `Uint8 -> "8 bit Unsigned Integer"
      end

  let rec pp_layout ppf = function
    | Zero_width ->
        Format.fprintf ppf "Zero width data, not actually present in the encoding"
    | Int integer ->
        Format.fprintf ppf "%a" pp_int integer
    | Bool ->
        Format.fprintf ppf "8 bit Signed Integer, with 0 for false and 255 for true"
    | RangedInt (minimum, maximum) ->
        Format.fprintf ppf "%a in the range %d to %d"
          pp_int ((Binary_size.range_to_size ~minimum ~maximum) :> integer_extended) minimum maximum
    | RangedFloat (minimum, maximum) ->
        Format.fprintf ppf
          "Double precision (8 byte) floating point number in the range %f to %f"
          minimum maximum
    | Float ->
        Format.fprintf ppf "Double precision (8 byte) floating point number"
    | Bytes ->
        Format.fprintf ppf "Bytes"
    | String ->
        Format.fprintf ppf "String"
    | Ref reference ->
        Format.fprintf ppf "%s" reference
    | Enum (size, reference) ->
        Format.fprintf ppf "%a encoding an enumeration (see %s)"
          pp_int (size :> integer_extended)
          reference
    | Seq (Ref reference) -> Format.fprintf ppf "Sequence of %s" reference
    | Seq data -> Format.fprintf ppf "Sequence of %a" pp_layout data


  let binary_table_headers = [ "Name" ; "Kind" ; "Data" ]
  let enum_headers = [ "Case number" ; "Encoded string" ]

  let pp_tag_size ppf tag =
    Format.fprintf ppf "%s" @@
    match tag with
    | `Uint8 -> "8 bit"
    | `Uint16 -> "16 bit"

  let field_descr () =
    let reference = ref 0 in
    let string_of_layout = Format.asprintf "%a" pp_layout  in
    let anon_num () =
      let value = !reference in
      reference := value + 1;
      string_of_int value in
    function
    | Named_field (name, kind, desc) ->
        [ name ; Format.asprintf "%a" pp_kind kind ; string_of_layout desc ]
    | Dynamic_field i ->
        [ Format.asprintf "Size of next %d fields" i ;
          Format.asprintf "%a" pp_kind (`Fixed 4) ; string_of_layout (Int `Int32) ]
    | Anonymous_field (kind, desc) ->
        [ "Unnamed field " ^ anon_num () ;
          Format.asprintf "%a" pp_kind kind ;
          string_of_layout desc ]
    | Option_indicator_field name ->
        [ "Presence of " ^ name ;
          Format.asprintf "%a" pp_kind (`Fixed 1) ;
          "0 if not present and 1 if present" ]

  let toplevel ({ name ; description }, encoding) =
    match encoding with
    | Obj { fields } ->
        Table { title = Format.asprintf "%s" name ;
                description ;
                headers = binary_table_headers ;
                body = List.map (field_descr ()) fields }
    | Cases { kind ; tag_size ; cases } ->
        Union (Format.asprintf "%s (%a, %a tag)" name pp_kind kind pp_tag_size tag_size,
               description, tag_size,
               List.map
                 (fun (tag, name, fields) ->
                    { title =
                        begin
                          match name with
                          | Some name -> Format.asprintf "%s (tag %d)" name tag
                          | None -> Format.asprintf "Tag %d" tag
                        end;
                      description = None ;
                      headers = binary_table_headers ;
                      body = List.map (field_descr ()) fields })
                 cases)
    | Int_enum { size ; cases } ->
        Table
          { title = Format.asprintf "Enum %s (%a):" name pp_int (size :> integer_extended) ;
            description = None;
            headers = enum_headers ;
            body = List.map (fun (num, str) -> [ string_of_int num ; str ]) cases }

  let to_print_ast encodings =
    List.map toplevel encodings

  let rec pad char ppf = function
    | 0 -> ()
    | n ->
        Format.pp_print_char ppf char ;
        pad char ppf (n - 1)

  let pp_table ppf (level, { title ; description ; headers ; body }) =
    let max_widths =
      List.fold_left (List.map2 (fun len str -> max (String.length str) len))
        (List.map String.length headers)
        body in
    let pp_row pad_char ppf =
      Format.fprintf ppf "|%a"
        (fun ppf ->
           List.iter2
             (fun width str -> Format.fprintf ppf " %s%a |" str (pad pad_char) (width - (String.length str)))
             max_widths) in
    let pp_option_nl ppf =
      Option.iter ~f:(Format.fprintf ppf "@,%s") in
    Format.fprintf ppf "@[<v 0>%a %s%a@,@,%a@,%a@,%a@,@]"
      (pad '#') level
      title
      pp_option_nl description
      (pp_row ' ') headers
      (pp_row '-') (List.map (fun _ -> "-") headers)
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,")
         (pp_row ' '))
      body

  let pp_print_structure ?(initial_level=0) ppf = function
    | Table table -> pp_table ppf (1 + initial_level, table)
    | Union (name, description, _tag_size, tables) ->
        Format.fprintf ppf "@[<v 0>%a %s:%a@,%a@]"
          (pad '#') (initial_level + 1)
          name
          (fun ppf -> function
             | None -> ()
             | Some description ->
                 Format.fprintf ppf "@,%s" description)
          description
          (fun ppf -> Format.pp_print_list
              ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,")
              pp_table
              ppf)
          (List.map (fun x -> (initial_level + 2, x)) tables)

  let pp ppf descrs =
    Format.pp_print_list
      ~pp_sep:(fun ppf () -> Format.fprintf ppf "@,")
      (pp_print_structure ~initial_level:0)
      ppf
      (to_print_ast descrs)

end

module Encoding = struct

  let description_encoding =
    conv
      (fun { name ; description } -> (name, description))
      (fun (name, description) -> { name ; description })
      (obj2
         (req "name" string)
         (opt "description" string))


  let integer_cases =
    [ ("Int16", `Int16) ;
      ("Int8", `Int8) ;
      ("Uint16", `Uint16) ;
      ("Uint8", `Uint8) ]

  let integer_encoding : Binary_size.integer encoding =
    string_enum integer_cases

  let integer_extended_encoding =
    string_enum
      (("Int64", `Int64) ::
       ("Int32", `Int32) ::
       integer_cases)

  let layout_encoding =
    mu "layout"
      (fun layout ->
         union [
           case
             ~name:"Zero_width"
             (Tag 0)
             (obj1
                (req "kind" (constant "Zero_width")))
             (function
               | Zero_width -> Some ()
               | _ -> None)
             (fun () -> Zero_width) ;
           case ~name:"Int"
             (Tag 1)
             (obj2
                (req "size" integer_extended_encoding)
                (req "kind" (constant "Int")))
             (function
               | Int integer -> Some (integer, ())
               | _ -> None)
             (fun (integer, _)-> Int integer) ;
           case ~name:"Bool"
             (Tag 2)
             (obj1 (req "kind" (constant "Bool")))
             (function
               | Bool -> Some ()
               | _ -> None)
             (fun () -> Bool) ;
           case ~name:"RangedInt"
             (Tag 3)
             (obj3
                (req "min" int31)
                (req "max" int31)
                (req "kind" (constant "RangedInt")))
             (function
               | RangedInt (min, max) -> Some (min, max, ())
               | _ -> None)
             (fun (min, max, _) -> RangedInt (min, max)) ;
           case ~name:"RangedFloat"
             (Tag 4)
             (obj3
                (req "min" float)
                (req "max" float)
                (req "kind" (constant "RangedFloat")))
             (function
               | RangedFloat (min, max) -> Some (min, max, ())
               | _ -> None)
             (fun (min, max, ()) -> RangedFloat (min, max)) ;
           case ~name:"Float"
             (Tag 5)
             (obj1 (req "kind" (constant "Float")))
             (function
               | Float -> Some ()
               | _ -> None)
             (fun () -> Float) ;
           case ~name:"Bytes"
             (Tag 6)
             (obj1 (req "kind" (constant "Bytes")))
             (function
               | Bytes -> Some ()
               | _ -> None)
             (fun () -> Bytes) ;
           case ~name:"String"
             (Tag 7)
             (obj1 (req "kind" (constant "String")))
             (function
               | String -> Some ()
               | _ -> None)
             (fun () -> String) ;
           case ~name:"Enum"
             (Tag 8)
             (obj3
                (req "size" integer_encoding)
                (req "reference" string)
                (req "kind" (constant "Enum")))
             (function
               | Enum (size, cases) -> Some (size, cases, ())
               | _ -> None)
             (fun (size, cases, _) -> Enum (size, cases)) ;
           case ~name:"Seq"
             (Tag 9)
             (obj2
                (req "layout" layout)
                (req "kind" (constant "Seq")))
             (function
               | Seq layout -> Some (layout, ())
               | _ -> None)
             (fun (layout, ()) -> Seq layout) ;
           case ~name:"Ref"
             (Tag 10)
             (obj2
                (req "name" string)
                (req "kind" (constant "Float")))
             (function
               | Ref layout -> Some (layout, ())
               | _ -> None)
             (fun (name, ()) -> Ref name)
         ])

  let kind_enum_cases =
    (fun () ->
       [ case ~name:"Dynamic"
           (Tag 0)
           (obj1 (req "kind" (constant "Dynamic")))
           (function `Dynamic -> Some ()
                   | _ -> None)
           (fun () -> `Dynamic) ;
         case ~name:"Variable"
           (Tag 1)
           (obj1 (req "kind" (constant "Variable")))
           (function `Variable -> Some ()
                   | _ -> None)
           (fun () -> `Variable) ])

  let kind_enum_encoding =
    def "schema.kind.enum" @@ union (kind_enum_cases ())

  let kind_t_encoding =
    def "schema.kind" @@
    union
      ((case ~name:"Fixed"
          (Tag 2)
          (obj2
             (req "size" int31)
             (req "kind" (constant "Float")))
          (function `Fixed n -> Some (n, ())
                  | _ -> None)
          (fun (n, _) -> `Fixed n)) :: (kind_enum_cases ()))

  let field_descr_encoding =
    let dynamic_layout_encoding = dynamic_size layout_encoding in
    def "schema.field" @@
    union [
      case ~name:"Named_field"
        (Tag 0)
        (obj4
           (req "name" string)
           (req "layout" dynamic_layout_encoding)
           (req "data_kind" kind_t_encoding)
           (req "kind" (constant "named")))
        (function Named_field (name, kind, layout) -> Some (name, layout, kind, ())
                | _ -> None)
        (fun (name, kind, layout, _) -> Named_field (name, layout, kind)) ;
      case ~name:"Anonymous_field"
        (Tag 1)
        (obj3
           (req "layout" dynamic_layout_encoding)
           (req "kind" (constant "anon"))
           (req "data_kind" kind_t_encoding))
        (function Anonymous_field (kind, layout) -> Some (layout, (), kind)
                | _ -> None)
        (fun (kind, _, layout) -> Anonymous_field (layout, kind)) ;
      case ~name:"Dynamic_field"
        (Tag 2)
        (obj2
           (req "kind" (constant "dyn"))
           (req "num_fields" int31))
        (function Dynamic_field i -> Some ((), i)
                | _ -> None)
        (fun ((), i) -> Dynamic_field i) ;
      case ~name:"Option_indicator_field"
        (Tag 3)
        (obj2
           (req "kind" (constant "option_indicator"))
           (req "name" string))
        (function Option_indicator_field s -> Some ((), s)
                | _ -> None)
        (fun ((), s) -> Option_indicator_field s)
    ]

  let tag_size_encoding =
    string_enum
      [("Uint16", `Uint16) ;
       ("Uint8", `Uint8) ]

  let binary_description_encoding =
    union [
      case ~name:"Obj"
        (Tag 0)
        (obj1
           (req "fields" (list (dynamic_size field_descr_encoding))))
        (function
          | Obj { fields } -> Some (fields)
          | _ -> None)
        (fun (fields) -> Obj { fields }) ;
      case ~name:"Cases"
        (Tag 1)
        (obj3
           (req "tag_size" tag_size_encoding)
           (req "kind" (dynamic_size kind_t_encoding))
           (req "cases"
              (list
                 (def "union case" @@
                  conv
                    (fun (tag, name, fields) -> (tag, fields, name))
                    (fun (tag, fields, name) -> (tag, name, fields)) @@
                  obj3
                    (req "tag" int31)
                    (req "fields" (list (dynamic_size field_descr_encoding)))
                    (opt "name" string)))))
        (function
          | Cases { kind ; tag_size ; cases } ->
              Some (tag_size, kind, cases)
          | _ -> None)
        (fun (tag_size, kind, cases) ->
           Cases { kind ; tag_size ; cases }) ;
      case ~name:"Int_enum"
        (Tag 2)
        (obj2
           (req "size" integer_encoding)
           (req "cases" (list (tup2 int31 string))))
        (function Int_enum { size ; cases } -> Some (size, cases)
                | _ -> None)
        (fun (size, cases) -> Int_enum { size ; cases })
    ]

  let encoding =
    list
      (obj2
         (req "description" description_encoding)
         (req "encoding" binary_description_encoding))

end

let encoding = Encoding.encoding
let pp = Printer.pp
