

;
;        /!\ /!\ Do not modify this file /!\ /!\
;
; but the original template in `tezos-protocol-compiler`
;


(rule
 (targets environment.ml)
 (action
  (write-file %{targets}
              "include Tezos_protocol_environment_shell.MakeV1(struct let name = \"000-Ps9mPmXa\" end)()
      module CamlinternalFormatBasics = struct include CamlinternalFormatBasics end
")))

(rule
 (targets registerer.ml)
 (deps tezos_embedded_protocol_environment_000_Ps9mPmXa.cmxa
       (:src_dir TEZOS_PROTOCOL))
 (action
  (with-stdout-to %{targets}
                  (chdir %{workspace_root} (run %{bin:tezos-embedded-protocol-packer} "%{src_dir}" "000_Ps9mPmXa")))))


(rule
 (targets functor.ml)
 (deps data.ml services.ml main.mli main.ml
       (:src_dir TEZOS_PROTOCOL))
 (action (with-stdout-to %{targets}
                         (chdir %{workspace_root}
                                (run %{bin:tezos-protocol-compiler.tezos-protocol-packer} %{src_dir})))))

(library
 (name tezos_protocol_000_Ps9mPmXa)
 (public_name tezos-protocol-000-Ps9mPmXa)
 (libraries tezos-protocol-environment-sigs)
 (flags -w "+a-4-6-7-9-29-40..42-44-45-48"
        -warn-error "-a+8"
        -safe-string -nopervasives)
 (modules Functor))

(library
 (name tezos_embedded_protocol_environment_000_Ps9mPmXa)
 (public_name tezos-embedded-protocol-000-Ps9mPmXa.environment)
 (library_flags (:standard -linkall))
 (libraries tezos-protocol-environment-shell)
 (modules Environment))

(library
 (name tezos_embedded_raw_protocol_000_Ps9mPmXa)
 (public_name tezos-embedded-protocol-000-Ps9mPmXa.raw)
 (libraries tezos_embedded_protocol_environment_000_Ps9mPmXa)
 (library_flags (:standard -linkall))
 (flags (:standard -nopervasives -nostdlib -safe-string
                   -w +a-4-6-7-9-29-32-40..42-44-45-48
                   -warn-error -a+8
                   -open Tezos_embedded_protocol_environment_000_Ps9mPmXa__Environment
                   -open Pervasives
                   -open Error_monad))
 (modules Data Services Main))

(library
 (name tezos_embedded_protocol_000_Ps9mPmXa)
 (public_name tezos-embedded-protocol-000-Ps9mPmXa)
 (library_flags (:standard -linkall))
 (libraries tezos_embedded_raw_protocol_000_Ps9mPmXa
            tezos-protocol-updater
            tezos-protocol-environment-shell)
 (flags (:standard -w +a-4-6-7-9-29-32-40..42-44-45-48
                   -warn-error -a+8))
 (modules Registerer))

(alias
 (name runtest_sandbox)
 (deps .tezos_protocol_000_Ps9mPmXa.objs/tezos_protocol_000_Ps9mPmXa.cmx))

