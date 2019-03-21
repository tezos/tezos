**************************
Online OCaml Documentation
**************************

.. raw:: html

    <iframe id="docframe" height="100%" width="100%" src="../api/odoc/_html/index.html"></iframe>
    <style>
    @media (max-width: 768px) {
         #docframe { border:none; position: fixed; top: 70px; bottom: 0; right: 0; left: 0; }
    }
    @media (min-width: 769px) {
         #docframe { border:none; position: fixed; top: 0px; bottom: 0; right: 0; left: 300px; }
    }
    </style>
    <script language="JavaScript">
      if (window.location.hash.endsWith(".html"))
        document.getElementById('docframe').src = "../api/odoc/_html/" + window.location.hash.slice(1)
    </script>
