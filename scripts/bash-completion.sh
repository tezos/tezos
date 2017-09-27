_tezos-client_complete()
{
    local cur_word prev_word type_list
    
    cur_word="${COMP_WORDS[COMP_CWORD]}"
    prev_word="${COMP_WORDS[COMP_CWORD-1]}"

    # Tezos script
    script=${COMP_WORDS[0]}
    reply=$($script bash_autocomplete "$prev_word" "$cur_word" ${COMP_WORDS[@]})
    
    COMPREPLY=($(compgen -W "$reply"))

    return 0
}

# Register _pss_complete to provide completion for the following commands
complete -F _tezos-client_complete tezos-client
