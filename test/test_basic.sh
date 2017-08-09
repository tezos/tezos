#!/bin/bash

set -e

source test_utils.sh

start_sandboxed_node
sleep 3
activate_alpha

add_bootstrap_identities

${TZCLIENT} list known identities

${TZCLIENT} transfer 1000 from bootstrap1 to ${KEY1}
${TZCLIENT} transfer 2000 from bootstrap1 to ${KEY2}

${TZCLIENT} get balance for ${KEY1} | assert "1,000.00 ꜩ"
${TZCLIENT} get balance for ${KEY2} | assert "2,000.00 ꜩ"

${TZCLIENT} transfer 1000 from ${KEY2} to ${KEY1}

${TZCLIENT} get balance for ${KEY1} | assert "2,000.00 ꜩ"
${TZCLIENT} get balance for ${KEY2} | assert "999.95 ꜩ"

# Should fail
# ${TZCLIENT} transfer 999.95 from ${KEY2} to ${KEY1}

${TZCLIENT} mine for bootstrap1

${TZCLIENT} remember program noop file:contracts/noop.tz
${TZCLIENT} typecheck program noop
${TZCLIENT} originate contract noop \
          for ${KEY1} transferring 1000 from bootstrap1 \
          running noop
${TZCLIENT} transfer 10 from bootstrap1 to noop -arg "Unit"

${TZCLIENT} originate contract hardlimit \
          for ${KEY1} transferring 1000 from bootstrap1 \
          running file:contracts/hardlimit.tz -init "3"
${TZCLIENT} transfer 10 from bootstrap1 to hardlimit -arg "Unit"
${TZCLIENT} transfer 10 from bootstrap1 to hardlimit -arg "Unit"
# ${TZCLIENT} transfer 10 from bootstrap1 to hardlimit -arg "unit" # should fail

${TZCLIENT} originate free account free_account for ${KEY1}
${TZCLIENT} get delegate for free_account
${TZCLIENT} set delegate for free_account to ${KEY2}
${TZCLIENT} get delegate for free_account

echo
echo End of test
echo
