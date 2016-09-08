COMMAND='gnome-terminal'
COUNT=2
for i in $(seq 1 $COUNT)
do
    SUBCOMMAND="./tezos-node -net-port $((9900 + i)) -net-local-discovery true -rpc-port $((8800 + i)) -net-expected-connections $(($COUNT - 1))  -base-dir /tmp/tezos_$i"
    COMMAND="$COMMAND --tab -e '$SUBCOMMAND'"
done
echo $COMMAND
eval $COMMAND
