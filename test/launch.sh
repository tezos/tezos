COMMAND='gnome-terminal'
COUNT=2
for i in $(seq 1 $COUNT)
do
    SUBCOMMAND="./tezos-node --net-addr :::$((9900 + i)) --local-discovery :::7732 --rpc-addr :::$((8800 + i)) --expected-connections $(($COUNT - 1)) --data-dir /tmp/tezos_$i"
    COMMAND="$COMMAND --tab -e '$SUBCOMMAND'"
done
echo $COMMAND
eval $COMMAND
