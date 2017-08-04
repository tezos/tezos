# Michelson Emacs mode
This mode is a work in progress.
Please contact us with bugs and feature requests.
All of the options mentioned below are also accessible via the customize menu.

## Required Configuration
To use the mode, you must load the `michelson-mode.el` file into Emacs.
Add the following to your `.emacs` file.
```elisp
(load "~/tezos/tezos/emacs/michelson-mode.el" nil t)
```

Before using the Emacs mode, you must configure the `michelson-client-command`.
If you have compiled the Tezos Git repository,
set this to be the path to the `tezos-client` binary on your system.
Make sure you have an up to date version of the client compiled.
You must also start a tezos node to enable typechecking features.
This option is recommended because it is faster than operating through
the docker container.

If you wish to run the Emacs mode with the alphanet script,
use the path of the `alphanet.sh` script, plus the word `client`.
You must also set the `michelson-alphanet` variable to be `t`.
If you do not set this option, the mode will not work with the alphanet.

Here are examples of the client configuration:
### Without the alphanet
```elisp
(setq michelson-client-command "~/tezos/tezos/tezos-client")
(setq michelson-alphanet nil)
```
### With the alphanet
```elisp
(setq michelson-client-command "~/tezos/alphanet/alphanet.sh client")
(setq michelson-alphanet t)
```

## Additional Options
There are various feature of the Emacs mode you may wish to configure.

### Error display
When writing a contract, you may wish to disable error display in order to
avoid the "wrong stack type at end of body" error that is often present.
This can be done by changing the
`michelson-print-errors` and `michelson-highlight-errors` options.
Both of these options also have interactive toggles for easy access.

### Live printing
You can disable live printing using the `michelson-live-editing` option.
If this option is disabled, both type and error printing are supressed.
No background command will be run, limiting the mode to syntax highlighting.
This command can also be toggled interactively using the
`michelson-toggle-live-editing` command.

### Faces
The highlighting colors used can be configured. See the customize menu for details.
