(*****************************************************************************)
(*                                                                           *)
(* Open Source License                                                       *)
(* Copyright (c) 2018 Dynamic Ledger Solutions, Inc. <contact@tezos.com>     *)
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

let pp_title ~char ppf title =
  let sub = String.map (fun _ -> char) title in
  Format.fprintf ppf "@[<v 0>%s@ %s@ @ @]" title sub

let pp_h1 = pp_title ~char:'#'
let pp_h2 = pp_title ~char:'*'
let pp_h3 = pp_title ~char:'='
let pp_h4 = pp_title ~char:'`'

let pp_raw_html ppf str =
  Format.fprintf ppf "@[<v>.. raw:: html@   @   %s@ @ @]"
    (Re.Str.global_replace (Re.Str.regexp "\n") "\n  " str)

let pp_html ppf f =
  Format.fprintf ppf
    "@[<v 2>.. raw:: html@ @ %a@]@\n@\n"
    (fun ppf () -> f ppf) ()

let pp_ref ppf name = Format.fprintf ppf ".. _%s :@\n@\n" name



let style = {css|
<style>
   .wy-nav-content {
      max-width: 100%;
   }
  .tab {
    overflow: hidden;
    border: 1px solid #ccc;
    background-color: #f1f1f1;
  }
  .tab button {
    background-color: inherit;
    float: left;
    border: none;
    outline: none;
    cursor: pointer;
    padding: 5px 10px;
  }
  .tab button:hover {
    background-color: #ddd;
  }
  .tab button.active {
    background-color: #ccc;
  }
  .tabcontent {
    display: none;
    padding: 6px 12px;
    border: 1px solid #ccc;
    border-top: none;
    max-height: 40ex;
    margin-bottom: 7ex;
    overflow: auto;
  }
  .tabcontent p {
    margin-bottom: 12px;
  }
  pre {
    font-size: 12px
  }
  .rst-content .section ul p {
    margin-bottom: 0;
  }
  span.query {
    font-family: monospace;
    white-space: pre;
  }
</style>
|css}

let script = {script|
<script>
  function showTab(elt, tab, ref) {
    var i, tabcontent, tablinks;
    tabcontent = document.getElementsByClassName(ref);
    for (i = 0; i < tabcontent.length; i++) {
      tabcontent[i].style.display = 'none';
    }

    tablinks = elt.parentNode.children;
    for (i = 0; i < tablinks.length; i++) {
      tablinks[i].className = tablinks[i].className.replace(' active', '');
    }

    document.getElementById(tab).style.display = 'block';
    elt.className += ' active';
  }

  document.addEventListener('DOMContentLoaded', function() {
    var a = document.getElementsByClassName('defaultOpen');
    for (i = 0; i < a.length; i++) { a[i].click() }
  })
</script>
|script}
