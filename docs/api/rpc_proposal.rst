.. raw:: html
  
  
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
  


.. raw:: html
  
  
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
  

.. _rpc_changes_june_2018:

RPC API changes, pre-Betanet (June 2018)
########################################

This document describes the changes made to RPC services provided by the Tezos node in June 2018.

Summary
*******

* Usage of GET requests and query-string parameters when possible
* Easy access to the protocol-specific part of block headers and of operations
* New metadata for block headers (e.g. baker, ...) and for operations (e.g. internal transfers, ...)
* Easy access to delegate's data (list of delegating contracts, frozen balance, ...)
* Prefixed `/blocks` by `/chains/<chain_id>` for easier testing of protocol updates
* Explicit distinction between the listing RPCs and the monitoring RPCs
* Homogeneous error handling

Detailled changes
*****************

Modified RPCs
=============

Some RPCs were simply renamed but some has significant changes in the expected arguments or the returned JSON object. See the notes for details.

In the follwing table:

* `chain_id` is a symbolic chain identifier, e.g. `main` or `test`, or their equivalent Base58-encoded hash, e. g. `NetXzGDuYoAawjv`.
* `block_id` is a symbolic block identifier, e.g. `head` of `genesis`, or any Base58-encoded block hash, e.g. `BKicMfwtgtAL28iD2uZk12PpuS6i2pFysC3syVT44nMNLkGb4QA`. It might also be `head~n` or `<block_hash>~n` to denotes the `n`-th predecessor of `head` or `<block_hash>`.
* `contract_id` is either a implicit contract identifier, i.e. a Base58-encoded public jye hash `tz1btz2tcu8dTwPrtMpyzh1irGdGkAaSE4bY`, or an originated contract identifier, e.g. `TZ1fyLSGZ7n2QRynStXUf9ihQtH7k4t4vTLe`.
* `peer_id` is a cryptographic peer identifier, e.g. `idtpXYaRSBXe6KJU6rk1X9ninWt8Bz`.

+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| Old path                                                                | Notes            | New path                                                                 |
+=========================================================================+==================+==========================================================================+
|                                                                         | [#blocks_list]_  | GET /chains/<chain_id>/blocks                                            |
+                                                                         +------------------+--------------------------------------------------------------------------+
| /blocks                                                                 | [#blocks_mon]_   | GET /monitor/valid_blocks                                                |
+                                                                         +------------------+--------------------------------------------------------------------------+
|                                                                         | [#heads_mon]_    | GET /monitor/heads/<chain_id>                                            |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>                                                      | [#blocks]_       | GET /chains/<chain_id>/blocks/<block_id>                                 |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/chain_id                                             |                  | GET /chains/<chain_id>/chain_id                                          |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/complete/<prefix>                                    |                  | GET /chains/<chain_id>/blocks/<block_id>/helpers/complete/<prefix>       |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/fitness                                              |                  | GET /chains/<chain_id>/blocks/<block_id>/header/shell/fitness            |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/hash                                                 |                  | GET /chains/<chain_id>/blocks/<block_id>/hash                            |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/level                                                |                  | GET /chains/<chain_id>/blocks/<block_id>/header/shell/level              |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/operations                                           | [#operations]_   | GET /chains/<chain_id>/blocks/<block_id>/operations                      |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/preapply                                             | [#preapply]_     | POST /chains/<chain_id>/blocks/<block_id>/helpers/preapply/block         |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/predecessor                                          |                  | GET /chains/<chain_id>/blocks/<block_id>/header/shell/predecessor        |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/predecessors                                         | [#predecessors]_ | GET /chains/<chain_id>/blocks                                            |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
|                                                                         |                  | GET /chains/<chain_id>/blocks/<block_id>/metadata/protocol_hash          |
+ /blocks/<block_id>/protocol                                             + [#protocol]_     +--------------------------------------------------------------------------+
|                                                                         |                  | GET /chains/<chain_id>/blocks/<block_id>/metadata/next_protocol_hash     |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/constants                                      |                  | GET /chains/<chain_id>/blocks/<block_id>/context/constants               |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/context/contracts                              |                  | GET /chains/<chain_id>/blocks/<block_id>/context/contracts               |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/context/contracts/<contract_id>                |                  | GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id> |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/context/level                                  | [#level]_        | GET /chains/<chain_id>/blocks/<block_id>/metadata/protocol_data          |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/context/next_level                             | [#next_level]_   | GET /chains/<chain_id>/blocks/<block_id>/helpers/current_level           |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/context/nonce/<block_level>                    |                  | GET /chains/<chain_id>/blocks/<block_id>/context/nonces/<block_level>    |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/context/voting_period_kind                     | [#level]_        | GET /chains/<chain_id>/blocks/<block_id>/metadata/protocol_data          |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/header                                         |                  | GET /chains/<chain_id>/blocks/<block_id>/header/protocol_data            |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/apply_operation                        | [#apply_op]_     | POST /chains/<chain_id>/blocks/<block_id>/helpers/preapply/operations    |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/forge/operations                       |                  | POST /chains/<chain_id>/blocks/<block_id>/helpers/forge/operations       |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/forge/protocol_data                    |                  | POST /chains/<chain_id>/blocks/<block_id>/helpers/forge/protocol_data    |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/hash_data                              |                  | POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/hash_data      |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/level/<block_level>                    | [#proto_level]_  | GET /chains/<chain_id>/blocks/<block_id>/helpers/current_level           |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/levels/<block_cycle>                   | [#proto_levels]_ | GET /chains/<chain_id>/blocks/<block_id>/helpers/levels_in_current_cycle |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/minimal_timestamp                      | [#rights_bake]_  | GET /chains/<chain_id>/blocks/<block_id>/helpers/baking_rights           |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/parse/block                            |                  | POST /chains/<chain_id>/blocks/<block_id>/helpers/parse/block            |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/parse/operations                       |                  | POST /chains/<chain_id>/blocks/<block_id>/helpers/parse/operations       |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/rights/baking                          | [#rights_bake]_  | GET /chains/<chain_id>/blocks/<block_id>/helpers/baking_rights           |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/rights/baking/delegate/<pkh>           | [#rights_bake]_  | GET /chains/<chain_id>/blocks/<block_id>/helpers/baking_rights           |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/rights/baking/level/<block_level>      | [#rights_bake]_  | GET /chains/<chain_id>/blocks/<block_id>/helpers/baking_rights           |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/rights/endorsement                     | [#rights_end]_   | GET /chains/<chain_id>/blocks/<block_id>/helpers/endorsing_rights        |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/rights/endorsement/delegate/<pkh>      | [#rights_end]_   | GET /chains/<chain_id>/blocks/<block_id>/helpers/endorsing_rights        |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/rights/endorsement/level/<block_level> | [#rights_end]_   | GET /chains/<chain_id>/blocks/<block_id>/helpers/endorsing_rights        |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/run_code                               |                  | POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/run_code       |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/trace_code                             |                  | POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/trace_code     |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/typecheck_code                         |                  | POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/typecheck_code |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/helpers/typecheck_data                         |                  | POST /chains/<chain_id>/blocks/<block_id>/helpers/scripts/typecheck_data |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/proto/operations                                     | [#operations]_   | GET /chains/<chain_id>/blocks/<block_id>/operations                      |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/raw_context                                          |                  | GET /chains/<chain_id>/blocks/<block_id>/context/raw                     |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/test_chain                                           |                  | GET /chains/<chain_id>/blocks/<block_id>/metadata/test_chain_status      |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /blocks/<block_id>/timestamp                                            |                  | GET /chains/<chain_id>/blocks/<block_id>/header/shell/timestamp          |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /bootstrapped                                                           |                  | GET /monitor/bootstrapped                                                |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /complete/<prefix>                                                      |                  | GET /chains/main/blocks/head/helpers/complete/<prefix>                   |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /describe                                                               |                  | GET /describe                                                            |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /errors                                                                 |                  | GET /errors                                                              |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /forge_block_header                                                     |                  | POST /chains/main/blocks/head/helpers/forge/block_header                 |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /inject_block                                                           |                  | POST /injection/block                                                    |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /inject_operation                                                       |                  | POST /injection/operation                                                |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /inject_protocol                                                        |                  | POST /injection/protocol                                                 |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /invalid_blocks                                                         |                  | GET /chains/<chain_id>/invalid_blocks                                    |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /invalid_blocks/<block_hash>                                            |                  | GET /chains/<chain_id>/invalid_blocks/<block_hash>                       |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /invalid_blocks/<block_hash>/unmark                                     |                  | DELETE /chains/<chain_id>/invalid_blocks/<block_hash>                    |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /mempool/pending_operations                                             |                  | GET /chains/<chain_id>/mempool/pending_operations                        |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/connections                                                        |                  | GET /network/connections                                                 |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/connections/<peer_id>                                              |                  | GET /network/connections/<peer_id>                                       |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/connections/<peer_id>/kick                                         |                  | DELETE /network/connections/<peer_id>                                    |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/connect/<point>                                                    |                  | PUT /network/points/<point>                                              |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/log                                                                |                  | GET /network/log                                                         |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/peers                                                              |                  | GET /network/peers/                                                      |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/peers/<peer_id>                                                    |                  | GET /network/peers/<peer_id>                                             |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/peers/<peer_id>/log                                                |                  | GET /network/peers/<peer_id>/log                                         |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /networks/point                                                         |                  | GET /network/points                                                      |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/points/<point>                                                     |                  | GET /network/points/<point>                                              |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/points/<point>/log                                                 |                  | GET /network/points/<point>/log                                          |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/stat                                                               |                  | GET /network/stat                                                        |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /p2p/versions                                                           |                  | GET /network/versions                                                    |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /protocols                                                              |                  | GET /protocols                                                           |
+                                                                         + [#protocols]_    +--------------------------------------------------------------------------+
|                                                                         |                  | GET /monitor/protocols                                                   |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /protocols/<protocol_hash>                                              |                  | GET /protocols/<protocol_hash>                                           |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /workers/block_validator                                                |                  | GET /workers/block_validator                                             |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /workers/chain_validators                                               |                  | GET /workers/chain_validators                                            |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /workers/chain_validators/<chain_id>                                    |                  | GET /workers/chain_validators/<chain_id>                                 |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /workers/peer_validators/<chain_id>                                     |                  | GET /workers/chain_validators/<chain_id>/peers_validators                |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /workers/peer_validators/<chain_id>/<peer_id>                           |                  | GET /workers/chain_validators/<chain_id>/peers_validators/<peer_id>      |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /workers/prevalidators                                                  |                  | GET /workers/prevalidators                                               |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+
| /workers/prevalidators/<chain_id>                                       |                  | GET /workers/prevalidators/<chain_id>                                    |
+-------------------------------------------------------------------------+------------------+--------------------------------------------------------------------------+

.. [#blocks] The return type of `/chain/<chain_id>/blocks/<block_id>` has changed. It now returns more informations:

  * the protocol-specific part of the block header is now fully parsed, instead of being returned as hex-encoded bytes. This allows for instance direct access to the block priority.

  * some metadata is also added: the baker, the current cycle, the current voting period, ...
  * the operations are also fully parsed, in the same way than the previous RPC `/blocks/<block_id>/proto/operations`
  * some metadata is also added to the operations. For instance, the list of created contracts, the list of internal transfers, the delegate associated to a endorsement, etc.

  The returned JSON being more detailled than previously, new RPCs for fetching partial information are added:

  * `/chain/<chain_id>/blocks/<block_id>/header`
  * `/chain/<chain_id>/blocks/<block_id>/metadata`
  * `/chain/<chain_id>/blocks/<block_id>/operations/<list_offset>/<operation_offset>`
  * `/chain/<chain_id>/blocks/<block_id>/operation_hashes/<list_offset>/<operation_offset>`

.. [#operations] Like the operations listed in `/chain/<chain_id>/blocks/<block_id>` [#blocks]_, the protocol-specific part of operations is now fully-parsed, instead of being returned as hex-encoded bytes.

   The same metadata is added: the list of created contracts, the list of internal transfers, the delegate associated to a endorsement, etc.

.. [#blocks_list] The old RPC `/blocks` has been splitted in three: one RPCs for listing heads and blocks, and two RPCs for monitoring new blocks.

   The new list RPCs only returns the hash of the current head.

.. [#blocks_mon] This new RPCs allows to monitor all valid blocks. It only returns the hash of the new blocks.

.. [#heads_mon] This new RPCs allows to only monitor the head changes in a given chain. It only returns the hash of the new heads.

.. [#level] The detailled level of a block (i.e. with the cycle and voting period details) is now included in the block metadata.

.. [#next_level] For the same behaviour, this RPC should be called with the `offset` query parameter: e.g. `/chains/main/blocks/head/helpers/current_level?offset=1`.

.. [#preapply] This function now expects a readable JSON object for the block header and the operations, instead of the hex-encoded sequence of bytes it was previously expecting.

.. [#apply_op] This function now expects a readable JSON object for the operation instead of the hex-encoded sequence of bytes it was previously expecting. It also accepts to apply a list of operation in sequence instead of a single operations.

   When the operations apply succesfully, this RPC now returns all the metadata associated to operation (the list of created contracts, the list of internal operations, ...) instead of just "success".

.. [#predecessors] For the same behaviour, this RPC should be called with the `length` query parameter: e.g. `/chains/main/blocks?head=<b58_block_hash>&length=12`.

.. [#proto_level] Instead of taking a level as parameter, this RPCs now expects the difference between the expected level and the current level. For instance, for querying the level details for the next block: `/chains/main/blocks/head/helpers/current_level?offset=1`.

.. [#proto_levels] Instead of taking a cycle as parameter, this RPCs now expects the difference between the expected cycle and the current cycle. For instance, for querying the levels in the previous cycle: `/chains/main/blocks/head/helpers/levels_in_current_cycle?offset=-1`.

.. [#protocol] To avoid confusion `protocol_hash` is the hash of protocol used to bake and validate the current block. And `next_protocol_hash` is the hash of the protocol that should be used to bake the next block. The previous RPC `protocol` was returning the `next_protocol_hash`.

.. [#protocols] The old RPC `/protocols` has been splitted in two: one RPCs for listing the known economic protocols, and one RPCs for monitoring new economic protocols. In both cases the new RPCs only returns hash of protocols instead of the protocol sources.

.. [#rights_bake] All the three RPCs used to query baking rights are now grouped in one RPCs. By default, this RPCs returns the first allowed bakers for the next block, in their priority order and with the time at which they are allowed to bake.

   Some additional query arguments allow to filter the answer with a given list of delegates, or to query informations for other levels or cycles (in that case the provided timestamp will only be an estimation).

.. [#rights_end] All the three RPCs used to query endorsing rights are now grouped in one RPCs. By default, this RPCs returns the list of allowed endorsers for the next block.

   Some additional query arguments allow to filter the answer with a limited list of delegates, or to query informations for others level or cycles.


New RPCs
========

* **GET /chains/<chain_id>/blocks/<block_id>/context/delegates**

  List all registered delegates.

* **GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>**

  List all information about a given delegate. This includes:

  * **GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>/balance**

    The full balance of a delegate: i.e. the balance of its delegate account plus the total amount of frozen deposits/rewards/fees.

  * **GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>/frozen_balance**

    The total amount of frozen deposits/rewards/fees.

  * **GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>/frozen_balance_by_cycle**

    The details of frozen deposits/rewards/fees indexed by the cycle at the end of which they will be unfrozen.

  * **GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>/staking_balance**

    The total amount of tokens the delegate stakes for. This includes the balance of all contracts delegated to <pkh>, but also the delegate own account and the frozen deposits and fees. This does not include the frozen rewards.

  * **GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>/delegated_contracts**

    The list of contracts that are delegated to <pkh>.

  * **GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>/delegated_balance**

    The total balance of all the contracts that delegates to <pkh>. This excludes the delegate own balance and its frozen balances.

  * **GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>/deactivated**

    Tells whether the delegate has been tagged as inactive by the system or not, i.e. a delegate is inactive when she did not baked or endorsed any block in the last 5 cycles.

    A deactivated delegate won't receive any baking or endorsing rights until she registers itself again as a delegate. No rolls were lost in the process, except if delegating contracts changed their delegate in the meantime of course.

  * **GET /chains/<chain_id>/blocks/<block_id>/context/delegates/<pkh>/grace_period**

    The cycle by the end of which the delegate will be tagged as deactivated if she did not bake or endorse until then.

* **GET /chains/<chain_id>/blocks/<block_id>/header**

  Simple accessor to a block header, or to its shell-specific or version-specific fragments.

  * **GET /chains/<chain_id>/blocks/<block_id>/header/shell**

  * **GET /chains/<chain_id>/blocks/<block_id>/header/protocol_data**

* **GET /chains/<chain_id>/blocks/<block_id>/metadata**

  Simple accessor to the metadata associated to a block header.

  * **GET /chains/<chain_id>/blocks/<block_id>/metadata/protocol_data**

    The version-specific part of the metadata. For the protocol Alpha this includes, for instance, the baker of the block.

  * **GET /chains/<chain_id>/blocks/<block_id>/metadata/protocol_hash**

    The hash of the protocol used to bake the block.

  * **GET /chains/<chain_id>/blocks/<block_id>/metadata/next_protocol_hash**

    The hash of the protocol that should be used to bake the next block.

* **GET /chains/<chain_id>/blocks/<block_id>/operation_hashes**

  The list of the hash of the operations included in the block.

  Also direct accessor to the `n-th` hash in the `m-th` validation pass.

  * **GET /chains/<chain_id>/blocks/<block_id>/operation_hashes/<list_offset>**

  * **GET /chains/<chain_id>/blocks/<block_id>/operation_hashes/<list_offset>/<operation_offset>**

* **GET /chains/<chain_id>/blocks/<block_id>/operations**

  The list of the operations included in the block, with their metadata.

  Also direct accessor to the `n-th` operation in the `m-th` validation pass.

  * **GET /chains/<chain_id>/blocks/<block_id>/operations/<list_offset>**

  * **GET /chains/<chain_id>/blocks/<block_id>/operations/<list_offset>/<operation_offset>**

* **GET /chains/<chain_id>/blocks/<block_id>/context/contracts/<contract_id>/manager_key**

  The public key of the contract's manager, if it has been revealed.



Consolidated Index
******************

Shell
=====

* /
  
  * /chains
    
    * /chains/<chain_id>
      
      * /chains/<chain_id>/blocks (`GET <GET_--chains--chain_id--blocks_>`_)
        
        * /chains/<chain_id>/blocks/<block_id> (<dyn>)
      
      * /chains/<chain_id>/chain_id (`GET <GET_--chains--chain_id--chain_id_>`_)
      
      * /chains/<chain_id>/invalid_blocks (`GET <GET_--chains--chain_id--invalid_blocks_>`_)
        
        * /chains/<chain_id>/invalid_blocks/<block_hash> (`GET <GET_--chains--chain_id--invalid_blocks--block_hash_>`_ `DELETE <DELETE_--chains--chain_id--invalid_blocks--block_hash_>`_)
      
      * /chains/<chain_id>/mempool (`GET <GET_--chains--chain_id--mempool_>`_)
  
  * /describe (`GET <GET_--describe_>`_)
  
  * /errors (`GET <GET_--errors_>`_)
  
  * /injection
    
    * /injection/block (`POST <POST_--injection--block_>`_)
    
    * /injection/operation (`POST <POST_--injection--operation_>`_)
    
    * /injection/protocol (`POST <POST_--injection--protocol_>`_)
  
  * /monitor
    
    * /monitor/bootstrapped (`GET <GET_--monitor--bootstrapped_>`_)
    
    * /monitor/heads
      
      * /monitor/heads/<chain_id> (`GET <GET_--monitor--heads--chain_id_>`_)
    
    * /monitor/protocols (`GET <GET_--monitor--protocols_>`_)
    
    * /monitor/valid_blocks (`GET <GET_--monitor--valid_blocks_>`_)
  
  * /network
    
    * /network/connections (`GET <GET_--network--connections_>`_)
      
      * /network/connections/<peer_id> (`GET <GET_--network--connections--peer_id_>`_ `DELETE <DELETE_--network--connections--peer_id_>`_)
    
    * /network/greylist
      
      * /network/greylist/clear (`GET <GET_--network--greylist--clear_>`_)
    
    * /network/log (`GET <GET_--network--log_>`_)
    
    * /network/peers (`GET <GET_--network--peers_>`_)
      
      * /network/peers/<peer_id> (`GET <GET_--network--peers--peer_id_>`_)
        
        * /network/peers/<peer_id>/ban (`GET <GET_--network--peers--peer_id--ban_>`_)
        
        * /network/peers/<peer_id>/banned (`GET <GET_--network--peers--peer_id--banned_>`_)
        
        * /network/peers/<peer_id>/log (`GET <GET_--network--peers--peer_id--log_>`_)
        
        * /network/peers/<peer_id>/trust (`GET <GET_--network--peers--peer_id--trust_>`_)
    
    * /network/points (`GET <GET_--network--points_>`_)
      
      * /network/points/<point> (`GET <GET_--network--points--point_>`_ `PUT <PUT_--network--points--point_>`_)
        
        * /network/points/<point>/ban (`GET <GET_--network--points--point--ban_>`_)
        
        * /network/points/<point>/banned (`GET <GET_--network--points--point--banned_>`_)
        
        * /network/points/<point>/log (`GET <GET_--network--points--point--log_>`_)
        
        * /network/points/<point>/trust (`GET <GET_--network--points--point--trust_>`_)
    
    * /network/stat (`GET <GET_--network--stat_>`_)
    
    * /network/versions (`GET <GET_--network--versions_>`_)
  
  * /protocols (`GET <GET_--protocols_>`_)
    
    * /protocols/<Protocol_hash> (`GET <GET_--protocols--Protocol_hash_>`_)
  
  * /workers
    
    * /workers/block_validator (`GET <GET_--workers--block_validator_>`_)
    
    * /workers/chain_validators (`GET <GET_--workers--chain_validators_>`_)
      
      * /workers/chain_validators/<chain_id> (`GET <GET_--workers--chain_validators--chain_id_>`_)
        
        * /workers/chain_validators/<chain_id>/peers_validators (`GET <GET_--workers--chain_validators--chain_id--peers_validators_>`_)
          
          * /workers/chain_validators/<chain_id>/peers_validators/<peer_id> (`GET <GET_--workers--chain_validators--chain_id--peers_validators--peer_id_>`_)
    
    * /workers/prevalidators (`GET <GET_--workers--prevalidators_>`_)
      
      * /workers/prevalidators/<chain_id> (`GET <GET_--workers--prevalidators--chain_id_>`_)

Protocol Alpha
==============

* ../<block_id> (`GET <GET_..--block_id_>`_)
  
  * ../<block_id>/context
    
    * ../<block_id>/context/constants (`GET <GET_..--block_id--context--constants_>`_)
      
      * ../<block_id>/context/constants/block_reward (`GET <GET_..--block_id--context--constants--block_reward_>`_)
      
      * ../<block_id>/context/constants/block_security_deposit (`GET <GET_..--block_id--context--constants--block_security_deposit_>`_)
      
      * ../<block_id>/context/constants/blocks_per_commitment (`GET <GET_..--block_id--context--constants--blocks_per_commitment_>`_)
      
      * ../<block_id>/context/constants/blocks_per_cycle (`GET <GET_..--block_id--context--constants--blocks_per_cycle_>`_)
      
      * ../<block_id>/context/constants/blocks_per_roll_snapshot (`GET <GET_..--block_id--context--constants--blocks_per_roll_snapshot_>`_)
      
      * ../<block_id>/context/constants/blocks_per_voting_period (`GET <GET_..--block_id--context--constants--blocks_per_voting_period_>`_)
      
      * ../<block_id>/context/constants/cost_per_byte (`GET <GET_..--block_id--context--constants--cost_per_byte_>`_)
      
      * ../<block_id>/context/constants/endorsement_reward (`GET <GET_..--block_id--context--constants--endorsement_reward_>`_)
      
      * ../<block_id>/context/constants/endorsement_security_deposit (`GET <GET_..--block_id--context--constants--endorsement_security_deposit_>`_)
      
      * ../<block_id>/context/constants/endorsers_per_block (`GET <GET_..--block_id--context--constants--endorsers_per_block_>`_)
      
      * ../<block_id>/context/constants/errors (`GET <GET_..--block_id--context--constants--errors_>`_)
      
      * ../<block_id>/context/constants/first_free_baking_slot (`GET <GET_..--block_id--context--constants--first_free_baking_slot_>`_)
      
      * ../<block_id>/context/constants/hard_gas_limits (`GET <GET_..--block_id--context--constants--hard_gas_limits_>`_)
      
      * ../<block_id>/context/constants/hard_storage_limits (`GET <GET_..--block_id--context--constants--hard_storage_limits_>`_)
      
      * ../<block_id>/context/constants/origination_burn (`GET <GET_..--block_id--context--constants--origination_burn_>`_)
      
      * ../<block_id>/context/constants/preserved_cycles (`GET <GET_..--block_id--context--constants--preserved_cycles_>`_)
      
      * ../<block_id>/context/constants/proof_of_work_threshold (`GET <GET_..--block_id--context--constants--proof_of_work_threshold_>`_)
      
      * ../<block_id>/context/constants/seed_nonce_revelation_tip (`GET <GET_..--block_id--context--constants--seed_nonce_revelation_tip_>`_)
      
      * ../<block_id>/context/constants/time_between_slots (`GET <GET_..--block_id--context--constants--time_between_slots_>`_)
    
    * ../<block_id>/context/contracts (`GET <GET_..--block_id--context--contracts_>`_)
      
      * ../<block_id>/context/contracts/<contract_id> (`GET <GET_..--block_id--context--contracts--contract_id_>`_)
        
        * ../<block_id>/context/contracts/<contract_id>/balance (`GET <GET_..--block_id--context--contracts--contract_id--balance_>`_)
        
        * ../<block_id>/context/contracts/<contract_id>/counter (`GET <GET_..--block_id--context--contracts--contract_id--counter_>`_)
        
        * ../<block_id>/context/contracts/<contract_id>/delegatable (`GET <GET_..--block_id--context--contracts--contract_id--delegatable_>`_)
        
        * ../<block_id>/context/contracts/<contract_id>/delegate (`GET <GET_..--block_id--context--contracts--contract_id--delegate_>`_)
        
        * ../<block_id>/context/contracts/<contract_id>/manager (`GET <GET_..--block_id--context--contracts--contract_id--manager_>`_)
        
        * ../<block_id>/context/contracts/<contract_id>/manager_key (`GET <GET_..--block_id--context--contracts--contract_id--manager_key_>`_)
        
        * ../<block_id>/context/contracts/<contract_id>/script (`GET <GET_..--block_id--context--contracts--contract_id--script_>`_)
        
        * ../<block_id>/context/contracts/<contract_id>/spendable (`GET <GET_..--block_id--context--contracts--contract_id--spendable_>`_)
        
        * ../<block_id>/context/contracts/<contract_id>/storage (`GET <GET_..--block_id--context--contracts--contract_id--storage_>`_)
    
    * ../<block_id>/context/delegates (`GET <GET_..--block_id--context--delegates_>`_)
      
      * ../<block_id>/context/delegates/<pkh> (`GET <GET_..--block_id--context--delegates--pkh_>`_)
        
        * ../<block_id>/context/delegates/<pkh>/balance (`GET <GET_..--block_id--context--delegates--pkh--balance_>`_)
        
        * ../<block_id>/context/delegates/<pkh>/deactivated (`GET <GET_..--block_id--context--delegates--pkh--deactivated_>`_)
        
        * ../<block_id>/context/delegates/<pkh>/delegated_balance (`GET <GET_..--block_id--context--delegates--pkh--delegated_balance_>`_)
        
        * ../<block_id>/context/delegates/<pkh>/delegated_contracts (`GET <GET_..--block_id--context--delegates--pkh--delegated_contracts_>`_)
        
        * ../<block_id>/context/delegates/<pkh>/frozen_balance (`GET <GET_..--block_id--context--delegates--pkh--frozen_balance_>`_)
        
        * ../<block_id>/context/delegates/<pkh>/frozen_balance_by_cycle (`GET <GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycle_>`_)
        
        * ../<block_id>/context/delegates/<pkh>/grace_period (`GET <GET_..--block_id--context--delegates--pkh--grace_period_>`_)
        
        * ../<block_id>/context/delegates/<pkh>/staking_balance (`GET <GET_..--block_id--context--delegates--pkh--staking_balance_>`_)
    
    * ../<block_id>/context/nonces
      
      * ../<block_id>/context/nonces/<block_level> (`GET <GET_..--block_id--context--nonces--block_level_>`_)
    
    * ../<block_id>/context/raw
      
      * ../<block_id>/context/raw/bytes (`GET <GET_..--block_id--context--raw--bytes_>`_)
      
      * ../<block_id>/context/raw/json (<dyn>)
  
  * ../<block_id>/hash (`GET <GET_..--block_id--hash_>`_)
  
  * ../<block_id>/header (`GET <GET_..--block_id--header_>`_)
    
    * ../<block_id>/header/protocol_data (`GET <GET_..--block_id--header--protocol_data_>`_)
    
    * ../<block_id>/header/shell (`GET <GET_..--block_id--header--shell_>`_)
      
      * ../<block_id>/header/shell/context_hash (`GET <GET_..--block_id--header--shell--context_hash_>`_)
      
      * ../<block_id>/header/shell/fitness (`GET <GET_..--block_id--header--shell--fitness_>`_)
      
      * ../<block_id>/header/shell/level (`GET <GET_..--block_id--header--shell--level_>`_)
      
      * ../<block_id>/header/shell/operations_hash (`GET <GET_..--block_id--header--shell--operations_hash_>`_)
      
      * ../<block_id>/header/shell/predecessor (`GET <GET_..--block_id--header--shell--predecessor_>`_)
      
      * ../<block_id>/header/shell/proto_level (`GET <GET_..--block_id--header--shell--proto_level_>`_)
      
      * ../<block_id>/header/shell/timestamp (`GET <GET_..--block_id--header--shell--timestamp_>`_)
      
      * ../<block_id>/header/shell/validation_passes (`GET <GET_..--block_id--header--shell--validation_passes_>`_)
  
  * ../<block_id>/helpers
    
    * ../<block_id>/helpers/baking_rights (`GET <GET_..--block_id--helpers--baking_rights_>`_)
    
    * ../<block_id>/helpers/complete
      
      * ../<block_id>/helpers/complete/<prefix> (`GET <GET_..--block_id--helpers--complete--prefix_>`_)
    
    * ../<block_id>/helpers/current_level (`GET <GET_..--block_id--helpers--current_level_>`_)
    
    * ../<block_id>/helpers/endorsing_rights (`GET <GET_..--block_id--helpers--endorsing_rights_>`_)
    
    * ../<block_id>/helpers/forge
      
      * ../<block_id>/helpers/forge/operations (`POST <POST_..--block_id--helpers--forge--operations_>`_)
      
      * ../<block_id>/helpers/forge/protocol_data (`POST <POST_..--block_id--helpers--forge--protocol_data_>`_)
    
    * ../<block_id>/helpers/forge_block_header (`POST <POST_..--block_id--helpers--forge_block_header_>`_)
    
    * ../<block_id>/helpers/levels_in_current_cycle (`GET <GET_..--block_id--helpers--levels_in_current_cycle_>`_)
    
    * ../<block_id>/helpers/parse
      
      * ../<block_id>/helpers/parse/block (`POST <POST_..--block_id--helpers--parse--block_>`_)
      
      * ../<block_id>/helpers/parse/operations (`POST <POST_..--block_id--helpers--parse--operations_>`_)
    
    * ../<block_id>/helpers/preapply
      
      * ../<block_id>/helpers/preapply/block (`POST <POST_..--block_id--helpers--preapply--block_>`_)
      
      * ../<block_id>/helpers/preapply/operations (`POST <POST_..--block_id--helpers--preapply--operations_>`_)
    
    * ../<block_id>/helpers/scripts
      
      * ../<block_id>/helpers/scripts/hash_data (`POST <POST_..--block_id--helpers--scripts--hash_data_>`_)
      
      * ../<block_id>/helpers/scripts/run_code (`POST <POST_..--block_id--helpers--scripts--run_code_>`_)
      
      * ../<block_id>/helpers/scripts/trace_code (`POST <POST_..--block_id--helpers--scripts--trace_code_>`_)
      
      * ../<block_id>/helpers/scripts/typecheck_code (`POST <POST_..--block_id--helpers--scripts--typecheck_code_>`_)
      
      * ../<block_id>/helpers/scripts/typecheck_data (`POST <POST_..--block_id--helpers--scripts--typecheck_data_>`_)
  
  * ../<block_id>/metadata (`GET <GET_..--block_id--metadata_>`_)
    
    * ../<block_id>/metadata/max_block_header_length (`GET <GET_..--block_id--metadata--max_block_header_length_>`_)
    
    * ../<block_id>/metadata/max_operation_data_length (`GET <GET_..--block_id--metadata--max_operation_data_length_>`_)
    
    * ../<block_id>/metadata/max_operations_ttl (`GET <GET_..--block_id--metadata--max_operations_ttl_>`_)
    
    * ../<block_id>/metadata/next_protocol_hash (`GET <GET_..--block_id--metadata--next_protocol_hash_>`_)
    
    * ../<block_id>/metadata/operation_list_quota (`GET <GET_..--block_id--metadata--operation_list_quota_>`_)
    
    * ../<block_id>/metadata/protocol_data (`GET <GET_..--block_id--metadata--protocol_data_>`_)
    
    * ../<block_id>/metadata/protocol_hash (`GET <GET_..--block_id--metadata--protocol_hash_>`_)
    
    * ../<block_id>/metadata/test_chain_status (`GET <GET_..--block_id--metadata--test_chain_status_>`_)
  
  * ../<block_id>/operation_hashes (`GET <GET_..--block_id--operation_hashes_>`_)
    
    * ../<block_id>/operation_hashes/<list_offset> (`GET <GET_..--block_id--operation_hashes--list_offset_>`_)
      
      * ../<block_id>/operation_hashes/<list_offset>/<operation_offset> (`GET <GET_..--block_id--operation_hashes--list_offset--operation_offset_>`_)
  
  * ../<block_id>/operations (`GET <GET_..--block_id--operations_>`_)
    
    * ../<block_id>/operations/<list_offset> (`GET <GET_..--block_id--operations--list_offset_>`_)
      
      * ../<block_id>/operations/<list_offset>/<operation_offset> (`GET <GET_..--block_id--operations--list_offset--operation_offset_>`_)

Consolidated descriptions
*************************

Shell
=====

.. _GET_--chains--chain_id--blocks :

**GET /chains/<chain_id>/blocks?[length=<int>]&(head=<block_hash>)\*&[min_date=<date>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--chains--chain_id--blocksdescr', 'GET_--chains--chain_id--blocks')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--chains--chain_id--blocksoutput', 'GET_--chains--chain_id--blocks')">Output format</button>
    </div><div id="GET_--chains--chain_id--blocksdescr" class="GET_--chains--chain_id--blocks tabcontent">
            <p>
            Lists known heads of the blockchain sorted with decreasing fitness. Optional arguments allows to returns the list of predecessors for known heads or the list of predecessors for a given list of blocks.</p> <p>Optional query arguments :<ul><li><span class="query">length = &lt;int&gt;</span> : The requested number of predecessors to returns (per requested head).</li><li><span class="query">head = &lt;block_hash&gt;</span> : An empty argument requests blocks from the current heads. A non empty list allow to request specific fragment of the chain.</li><li><span class="query">min_date = &lt;date&gt;</span> : When `min_date` is provided, heads with a timestamp before `min_date` are filtered out</li></ul></p>
            </div>
  <div id="GET_--chains--chain_id--blocksoutput" class="GET_--chains--chain_id--blocks tabcontent">
    <pre>
    [ [ string
    /* A block identifier (Base58Check-encoded) */ ... ] ... ]</pre>
    </div>
  


.. _GET_--chains--chain_id--chain_id :

**GET /chains/<chain_id>/chain_id**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--chains--chain_id--chain_iddescr', 'GET_--chains--chain_id--chain_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--chains--chain_id--chain_idoutput', 'GET_--chains--chain_id--chain_id')">Output format</button>
    </div><div id="GET_--chains--chain_id--chain_iddescr" class="GET_--chains--chain_id--chain_id tabcontent">
            <p>
            The chain unique identifier.</p>
            </div>
  <div id="GET_--chains--chain_id--chain_idoutput" class="GET_--chains--chain_id--chain_id tabcontent">
    <pre>
    string
    /* Network identifier (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_--chains--chain_id--invalid_blocks :

**GET /chains/<chain_id>/invalid_blocks**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--chains--chain_id--invalid_blocksdescr', 'GET_--chains--chain_id--invalid_blocks')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--chains--chain_id--invalid_blocksoutput', 'GET_--chains--chain_id--invalid_blocks')">Output format</button>
    </div><div id="GET_--chains--chain_id--invalid_blocksdescr" class="GET_--chains--chain_id--invalid_blocks tabcontent">
            <p>
            Lists blocks that have been declared invalid along with the errors that led to them being declared invalid.</p>
            </div>
  <div id="GET_--chains--chain_id--invalid_blocksoutput" class="GET_--chains--chain_id--invalid_blocks tabcontent">
    <pre>
    [ { "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "errors":
          any
          /* The full list of error is available with the global RPC `GET
             errors` */ } ... ]</pre>
    </div>
  

.. _GET_--chains--chain_id--invalid_blocks--block_hash :

**GET /chains/<chain_id>/invalid_blocks/<block_hash>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--chains--chain_id--invalid_blocks--block_hashdescr', 'GET_--chains--chain_id--invalid_blocks--block_hash')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--chains--chain_id--invalid_blocks--block_hashoutput', 'GET_--chains--chain_id--invalid_blocks--block_hash')">Output format</button>
    </div><div id="GET_--chains--chain_id--invalid_blocks--block_hashdescr" class="GET_--chains--chain_id--invalid_blocks--block_hash tabcontent">
            <p>
            The errors that appears during the block (in)validation.</p>
            </div>
  <div id="GET_--chains--chain_id--invalid_blocks--block_hashoutput" class="GET_--chains--chain_id--invalid_blocks--block_hash tabcontent">
    <pre>
    { "block": string /* A block identifier (Base58Check-encoded) */,
      "level": integer ∈ [-2^31-2, 2^31+2],
      "errors":
        any
        /* The full list of error is available with the global RPC `GET errors` */ }</pre>
    </div>
  

.. _DELETE_--chains--chain_id--invalid_blocks--block_hash :

**DELETE /chains/<chain_id>/invalid_blocks/<block_hash>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'DELETE_--chains--chain_id--invalid_blocks--block_hashdescr', 'DELETE_--chains--chain_id--invalid_blocks--block_hash')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'DELETE_--chains--chain_id--invalid_blocks--block_hashoutput', 'DELETE_--chains--chain_id--invalid_blocks--block_hash')">Output format</button>
    </div><div id="DELETE_--chains--chain_id--invalid_blocks--block_hashdescr" class="DELETE_--chains--chain_id--invalid_blocks--block_hash tabcontent">
            <p>
            Remove an invalid block for the tezos storage</p>
            </div>
  <div id="DELETE_--chains--chain_id--invalid_blocks--block_hashoutput" class="DELETE_--chains--chain_id--invalid_blocks--block_hash tabcontent">
    <pre>
    {  }</pre>
    </div>
  


.. _GET_--chains--chain_id--mempool :

**GET /chains/<chain_id>/mempool**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--chains--chain_id--mempooldescr', 'GET_--chains--chain_id--mempool')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--chains--chain_id--mempooloutput', 'GET_--chains--chain_id--mempool')">Output format</button>
    </div><div id="GET_--chains--chain_id--mempooldescr" class="GET_--chains--chain_id--mempool tabcontent">
            <p>
            List the not-yet-prevalidated operations.</p>
            </div>
  <div id="GET_--chains--chain_id--mempooloutput" class="GET_--chains--chain_id--mempool tabcontent">
    <pre>
    { "applied":
        [ { "hash": string /* A Tezos operation ID (Base58Check-encoded) */,
            "branch": string /* A block identifier (Base58Check-encoded) */,
            "data": /^[a-zA-Z0-9]+$/ } ... ],
      "refused":
        [ { "hash": string /* A Tezos operation ID (Base58Check-encoded) */,
            "branch": string /* A block identifier (Base58Check-encoded) */,
            "data": /^[a-zA-Z0-9]+$/,
            "error":
              any
              /* The full list of error is available with the global RPC `GET
                 errors` */ } ... ],
      "branch_refused":
        [ { "hash": string /* A Tezos operation ID (Base58Check-encoded) */,
            "branch": string /* A block identifier (Base58Check-encoded) */,
            "data": /^[a-zA-Z0-9]+$/,
            "error":
              any
              /* The full list of error is available with the global RPC `GET
                 errors` */ } ... ],
      "branch_delayed":
        [ { "hash": string /* A Tezos operation ID (Base58Check-encoded) */,
            "branch": string /* A block identifier (Base58Check-encoded) */,
            "data": /^[a-zA-Z0-9]+$/,
            "error":
              any
              /* The full list of error is available with the global RPC `GET
                 errors` */ } ... ],
      "unprocessed":
        [ { "hash": string /* A Tezos operation ID (Base58Check-encoded) */,
            "branch": string /* A block identifier (Base58Check-encoded) */,
            "data": /^[a-zA-Z0-9]+$/ } ... ] }</pre>
    </div>
  


.. _GET_--describe :

**GET /describe?[recurse=<bool>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--describedescr', 'GET_--describe')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--describeoutput', 'GET_--describe')">Output format</button>
    </div><div id="GET_--describedescr" class="GET_--describe tabcontent">
            <p>
            RPCs documentation and input/output schema</p> <p>Optional query arguments :<ul><li><span class="query">recurse = &lt;bool&gt;</span></li></ul></p>
            </div>
  <div id="GET_--describeoutput" class="GET_--describe tabcontent">
    <pre>
    $service_tree
    $service_tree:
      { "static":
          { "get_service"?:
              { "meth": "PATCH" | "GET" | "POST" | "PUT" | "DELETE",
                "path":
                  [ string
                    || { "id": "single",
                         "name": string,
                         "descr"?: string }
                    || { "id": "multiple",
                         "name": string,
                         "descr"?: string } ... ],
                "description"?: string,
                "query":
                  [ { "name": string,
                      "description"?: string,
                      "kind":
                        { "single":
                            { "id": "single",
                              "name": string,
                              "descr"?: string } }
                        || { "optional":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } }
                        || { "flag": {  } }
                        || { "multi":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } } } ... ],
                "input"?: any,
                "output": any,
                "erro": any },
            "post_service"?:
              { "meth": "PATCH" | "GET" | "POST" | "PUT" | "DELETE",
                "path":
                  [ string
                    || { "id": "single",
                         "name": string,
                         "descr"?: string }
                    || { "id": "multiple",
                         "name": string,
                         "descr"?: string } ... ],
                "description"?: string,
                "query":
                  [ { "name": string,
                      "description"?: string,
                      "kind":
                        { "single":
                            { "id": "single",
                              "name": string,
                              "descr"?: string } }
                        || { "optional":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } }
                        || { "flag": {  } }
                        || { "multi":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } } } ... ],
                "input"?: any,
                "output": any,
                "erro": any },
            "delete_service"?:
              { "meth": "PATCH" | "GET" | "POST" | "PUT" | "DELETE",
                "path":
                  [ string
                    || { "id": "single",
                         "name": string,
                         "descr"?: string }
                    || { "id": "multiple",
                         "name": string,
                         "descr"?: string } ... ],
                "description"?: string,
                "query":
                  [ { "name": string,
                      "description"?: string,
                      "kind":
                        { "single":
                            { "id": "single",
                              "name": string,
                              "descr"?: string } }
                        || { "optional":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } }
                        || { "flag": {  } }
                        || { "multi":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } } } ... ],
                "input"?: any,
                "output": any,
                "erro": any },
            "put_service"?:
              { "meth": "PATCH" | "GET" | "POST" | "PUT" | "DELETE",
                "path":
                  [ string
                    || { "id": "single",
                         "name": string,
                         "descr"?: string }
                    || { "id": "multiple",
                         "name": string,
                         "descr"?: string } ... ],
                "description"?: string,
                "query":
                  [ { "name": string,
                      "description"?: string,
                      "kind":
                        { "single":
                            { "id": "single",
                              "name": string,
                              "descr"?: string } }
                        || { "optional":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } }
                        || { "flag": {  } }
                        || { "multi":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } } } ... ],
                "input"?: any,
                "output": any,
                "erro": any },
            "patch_service"?:
              { "meth": "PATCH" | "GET" | "POST" | "PUT" | "DELETE",
                "path":
                  [ string
                    || { "id": "single",
                         "name": string,
                         "descr"?: string }
                    || { "id": "multiple",
                         "name": string,
                         "descr"?: string } ... ],
                "description"?: string,
                "query":
                  [ { "name": string,
                      "description"?: string,
                      "kind":
                        { "single":
                            { "id": "single",
                              "name": string,
                              "descr"?: string } }
                        || { "optional":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } }
                        || { "flag": {  } }
                        || { "multi":
                               { "id": "single",
                                 "name": string,
                                 "descr"?: string } } } ... ],
                "input"?: any,
                "output": any,
                "erro": any },
            "subdirs"?:
              { "suffixes": [ { "name": string,
                                "tree": $service_tree } ... ] }
              || { "dynamic_dispatch":
                     { "arg":
                         { "id": "single",
                           "name": string,
                           "descr"?: string },
                       "tree": $service_tree } } } }
      || { "dynamic": string || null }</pre>
    </div>
  


.. _GET_--errors :

**GET /errors**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--errorsdescr', 'GET_--errors')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--errorsoutput', 'GET_--errors')">Output format</button>
    </div><div id="GET_--errorsdescr" class="GET_--errors tabcontent">
            <p>
            Schema for all the RPC errors from the shell</p>
            </div>
  <div id="GET_--errorsoutput" class="GET_--errors tabcontent">
    <pre>
    any</pre>
    </div>
  


.. _POST_--injection--block :

**POST /injection/block?[async]&[force]&[chain=<chain_id>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_--injection--blockdescr', 'POST_--injection--block')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_--injection--blockinput', 'POST_--injection--block')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_--injection--blockoutput', 'POST_--injection--block')">Output format</button>
    </div><div id="POST_--injection--blockdescr" class="POST_--injection--block tabcontent">
            <p>
            Inject a block in the node and broadcast it. The `operations` embedded in `blockHeader` might be pre-validated using a contextual RPCs from the latest block (e.g. '/blocks/head/context/preapply'). Returns the ID of the block. By default, the RPC will wait for the block to be validated before answering.</p> <p>Optional query arguments :<ul><li><span class="query">async</span></li><li><span class="query">force</span></li><li><span class="query">chain = &lt;chain_id&gt;</span></li></ul></p>
            </div>
  <div id="POST_--injection--blockinput" class="POST_--injection--block tabcontent">
    <pre>
    { "data": /^[a-zA-Z0-9]+$/,
      "operations":
        [ [ { "branch": string /* A block identifier (Base58Check-encoded) */,
              "data": /^[a-zA-Z0-9]+$/ } ... ] ... ] }</pre>
    </div>
  <div id="POST_--injection--blockoutput" class="POST_--injection--block tabcontent">
    <pre>
    string
    /* A block identifier (Base58Check-encoded) */</pre>
    </div>
  


.. _POST_--injection--operation :

**POST /injection/operation?[async]&[chain=<chain_id>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_--injection--operationdescr', 'POST_--injection--operation')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_--injection--operationinput', 'POST_--injection--operation')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_--injection--operationoutput', 'POST_--injection--operation')">Output format</button>
    </div><div id="POST_--injection--operationdescr" class="POST_--injection--operation tabcontent">
            <p>
            Inject an operation in node and broadcast it. Returns the ID of the operation. The `signedOperationContents` should be constructed using a contextual RPCs from the latest block and signed by the client. By default, the RPC will wait for the operation to be (pre-)validated before answering. See RPCs under /blocks/prevalidation for more details on the prevalidation context.</p> <p>Optional query arguments :<ul><li><span class="query">async</span></li><li><span class="query">chain = &lt;chain_id&gt;</span></li></ul></p>
            </div>
  <div id="POST_--injection--operationinput" class="POST_--injection--operation tabcontent">
    <pre>
    /^[a-zA-Z0-9]+$/</pre>
    </div>
  <div id="POST_--injection--operationoutput" class="POST_--injection--operation tabcontent">
    <pre>
    string
    /* A Tezos operation ID (Base58Check-encoded) */</pre>
    </div>
  


.. _POST_--injection--protocol :

**POST /injection/protocol?[async]&[force]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_--injection--protocoldescr', 'POST_--injection--protocol')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_--injection--protocolinput', 'POST_--injection--protocol')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_--injection--protocoloutput', 'POST_--injection--protocol')">Output format</button>
    </div><div id="POST_--injection--protocoldescr" class="POST_--injection--protocol tabcontent">
            <p>
            Inject a protocol in node. Returns the ID of the protocol.</p> <p>Optional query arguments :<ul><li><span class="query">async</span></li><li><span class="query">force</span></li></ul></p>
            </div>
  <div id="POST_--injection--protocolinput" class="POST_--injection--protocol tabcontent">
    <pre>
    { "expected_env_version": integer ∈ [-2^15, 2^15-1],
      "components":
        [ { "name": string,
            "interface"?: /^[a-zA-Z0-9]+$/,
            "implementation": /^[a-zA-Z0-9]+$/ } ... ] }</pre>
    </div>
  <div id="POST_--injection--protocoloutput" class="POST_--injection--protocol tabcontent">
    <pre>
    string
    /* A Tezos protocol ID (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_--monitor--bootstrapped :

**GET /monitor/bootstrapped**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--monitor--bootstrappeddescr', 'GET_--monitor--bootstrapped')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--monitor--bootstrappedoutput', 'GET_--monitor--bootstrapped')">Output format</button>
    </div><div id="GET_--monitor--bootstrappeddescr" class="GET_--monitor--bootstrapped tabcontent">
            <p>
            </p>
            </div>
  <div id="GET_--monitor--bootstrappedoutput" class="GET_--monitor--bootstrapped tabcontent">
    <pre>
    { "block": string /* A block identifier (Base58Check-encoded) */,
      "timestamp":
        /* timestamp */
        $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _GET_--monitor--heads--chain_id :

**GET /monitor/heads/<chain_id>?(next_protocol=<Protocol_hash>)\***

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--monitor--heads--chain_iddescr', 'GET_--monitor--heads--chain_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--monitor--heads--chain_idoutput', 'GET_--monitor--heads--chain_id')">Output format</button>
    </div><div id="GET_--monitor--heads--chain_iddescr" class="GET_--monitor--heads--chain_id tabcontent">
            <p>
            </p> <p>Optional query arguments :<ul><li><span class="query">next_protocol = &lt;Protocol_hash&gt;</span></li></ul></p>
            </div>
  <div id="GET_--monitor--heads--chain_idoutput" class="GET_--monitor--heads--chain_id tabcontent">
    <pre>
    string
    /* A block identifier (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_--monitor--protocols :

**GET /monitor/protocols**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--monitor--protocolsdescr', 'GET_--monitor--protocols')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--monitor--protocolsoutput', 'GET_--monitor--protocols')">Output format</button>
    </div><div id="GET_--monitor--protocolsdescr" class="GET_--monitor--protocols tabcontent">
            <p>
            ...FIXME...</p>
            </div>
  <div id="GET_--monitor--protocolsoutput" class="GET_--monitor--protocols tabcontent">
    <pre>
    string
    /* A Tezos protocol ID (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_--monitor--valid_blocks :

**GET /monitor/valid_blocks?(protocol=<Protocol_hash>)\*&(next_protocol=<Protocol_hash>)\*&(chain=<chain_id>)\***

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--monitor--valid_blocksdescr', 'GET_--monitor--valid_blocks')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--monitor--valid_blocksoutput', 'GET_--monitor--valid_blocks')">Output format</button>
    </div><div id="GET_--monitor--valid_blocksdescr" class="GET_--monitor--valid_blocks tabcontent">
            <p>
            </p> <p>Optional query arguments :<ul><li><span class="query">protocol = &lt;Protocol_hash&gt;</span></li><li><span class="query">next_protocol = &lt;Protocol_hash&gt;</span></li><li><span class="query">chain = &lt;chain_id&gt;</span></li></ul></p>
            </div>
  <div id="GET_--monitor--valid_blocksoutput" class="GET_--monitor--valid_blocks tabcontent">
    <pre>
    { "chain_id": string /* Network identifier (Base58Check-encoded) */,
      "hash": string /* A block identifier (Base58Check-encoded) */ }</pre>
    </div>
  


.. _GET_--network--connections :

**GET /network/connections**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--connectionsdescr', 'GET_--network--connections')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--connectionsoutput', 'GET_--network--connections')">Output format</button>
    </div><div id="GET_--network--connectionsdescr" class="GET_--network--connections tabcontent">
            <p>
            List the running P2P connection.</p>
            </div>
  <div id="GET_--network--connectionsoutput" class="GET_--network--connections tabcontent">
    <pre>
    [ { "incoming": boolean,
        "peer_id": string /* A Cryptobox public key ID (Base58Check-encoded) */,
        "id_point": { "addr": string,
                      "port"?: integer ∈ [0, 2^16-1] },
        "remote_socket_port": integer ∈ [0, 2^16-1],
        "versions":
          [ { "name": string,
              "major": integer ∈ [0, 2^16-1],
              "minor": integer ∈ [0, 2^16-1] } ... ] } ... ]</pre>
    </div>
  

.. _GET_--network--connections--peer_id :

**GET /network/connections/<peer_id>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--connections--peer_iddescr', 'GET_--network--connections--peer_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--connections--peer_idoutput', 'GET_--network--connections--peer_id')">Output format</button>
    </div><div id="GET_--network--connections--peer_iddescr" class="GET_--network--connections--peer_id tabcontent">
            <p>
            Details about the current P2P connection to the given peer.</p>
            </div>
  <div id="GET_--network--connections--peer_idoutput" class="GET_--network--connections--peer_id tabcontent">
    <pre>
    { "incoming": boolean,
      "peer_id": string /* A Cryptobox public key ID (Base58Check-encoded) */,
      "id_point": { "addr": string,
                    "port"?: integer ∈ [0, 2^16-1] },
      "remote_socket_port": integer ∈ [0, 2^16-1],
      "versions":
        [ { "name": string,
            "major": integer ∈ [0, 2^16-1],
            "minor": integer ∈ [0, 2^16-1] } ... ] }</pre>
    </div>
  

.. _DELETE_--network--connections--peer_id :

**DELETE /network/connections/<peer_id>?[wait]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'DELETE_--network--connections--peer_iddescr', 'DELETE_--network--connections--peer_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'DELETE_--network--connections--peer_idoutput', 'DELETE_--network--connections--peer_id')">Output format</button>
    </div><div id="DELETE_--network--connections--peer_iddescr" class="DELETE_--network--connections--peer_id tabcontent">
            <p>
            Forced close of the current P2P connection to the given peer.</p> <p>Optional query arguments :<ul><li><span class="query">wait</span></li></ul></p>
            </div>
  <div id="DELETE_--network--connections--peer_idoutput" class="DELETE_--network--connections--peer_id tabcontent">
    <pre>
    {  }</pre>
    </div>
  


.. _GET_--network--greylist--clear :

**GET /network/greylist/clear**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--greylist--cleardescr', 'GET_--network--greylist--clear')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--greylist--clearoutput', 'GET_--network--greylist--clear')">Output format</button>
    </div><div id="GET_--network--greylist--cleardescr" class="GET_--network--greylist--clear tabcontent">
            <p>
            Clear all greylists tables.</p>
            </div>
  <div id="GET_--network--greylist--clearoutput" class="GET_--network--greylist--clear tabcontent">
    <pre>
    {  }</pre>
    </div>
  


.. _GET_--network--log :

**GET /network/log**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--logdescr', 'GET_--network--log')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--logoutput', 'GET_--network--log')">Output format</button>
    </div><div id="GET_--network--logdescr" class="GET_--network--log tabcontent">
            <p>
            Stream of all network events</p>
            </div>
  <div id="GET_--network--logoutput" class="GET_--network--log tabcontent">
    <pre>
    { "event": "too_few_connections" }
    || { "event": "too_many_connections" }
    || { "event": "new_point",
         "point": string }
    || { "event": "new_peer",
         "peer_id":
           string
           /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "incoming_connection",
         "point": string }
    || { "event": "outgoing_connection",
         "point": string }
    || { "event": "authentication_failed",
         "point": string }
    || { "event": "accepting_request",
         "point": string,
         "id_point": { "addr": string,
                       "port"?: integer ∈ [0, 2^16-1] },
         "peer_id":
           string
           /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "rejecting_request",
         "point": string,
         "id_point": { "addr": string,
                       "port"?: integer ∈ [0, 2^16-1] },
         "peer_id":
           string
           /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "request_rejected",
         "point": string,
         "identity"?:
           [ { "addr": string,
               "port"?: integer ∈ [0, 2^16-1] },
             string
             /* A Cryptobox public key ID (Base58Check-encoded) */ ] }
    || { "event": "connection_established",
         "id_point": { "addr": string,
                       "port"?: integer ∈ [0, 2^16-1] },
         "peer_id":
           string
           /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "disconnection",
         "peer_id":
           string
           /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "external_disconnection",
         "peer_id":
           string
           /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "gc_points" }
    || { "event": "gc_peer_ids" }
    || { "event": "swap_request_received",
         "source": string /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "swap_ack_received",
         "source": string /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "swap_request_sent",
         "source": string /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "swap_ack_sent",
         "source": string /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "swap_request_ignored",
         "source": string /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "swap_success",
         "source": string /* A Cryptobox public key ID (Base58Check-encoded) */ }
    || { "event": "swap_failure",
         "source": string /* A Cryptobox public key ID (Base58Check-encoded) */ }</pre>
    </div>
  


.. _GET_--network--peers :

**GET /network/peers?(filter=<p2p.point.state_filter>)\***

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--peersdescr', 'GET_--network--peers')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--peersoutput', 'GET_--network--peers')">Output format</button>
    </div><div id="GET_--network--peersdescr" class="GET_--network--peers tabcontent">
            <p>
            List the peers the node ever met.</p> <p>Optional query arguments :<ul><li><span class="query">filter = &lt;p2p.point.state_filter&gt;</span></li></ul></p>
            </div>
  <div id="GET_--network--peersoutput" class="GET_--network--peers tabcontent">
    <pre>
    [ [ string
        /* A Cryptobox public key ID (Base58Check-encoded) */,
        { "score": number,
          "trusted": boolean,
          "state": "running" | "accepted" | "disconnected",
          "reachable_at"?: { "addr": string,
                             "port"?: integer ∈ [0, 2^16-1] },
          "stat":
            { "total_sent": integer ∈ [-2^31-2, 2^31+2] || string,
              "total_recv": integer ∈ [-2^31-2, 2^31+2] || string,
              "current_inflow": integer ∈ [-2^30-2, 2^30+2],
              "current_outflow": integer ∈ [-2^30-2, 2^30+2] },
          "last_failed_connection"?:
            [ { "addr": string,
                "port"?: integer ∈ [0, 2^16-1] },
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
          "last_rejected_connection"?:
            [ { "addr": string,
                "port"?: integer ∈ [0, 2^16-1] },
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
          "last_established_connection"?:
            [ { "addr": string,
                "port"?: integer ∈ [0, 2^16-1] },
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
          "last_disconnection"?:
            [ { "addr": string,
                "port"?: integer ∈ [0, 2^16-1] },
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
          "last_seen"?:
            [ { "addr": string,
                "port"?: integer ∈ [0, 2^16-1] },
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
          "last_miss"?:
            [ { "addr": string,
                "port"?: integer ∈ [0, 2^16-1] },
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ] } ] ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_--network--peers--peer_id :

**GET /network/peers/<peer_id>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--peers--peer_iddescr', 'GET_--network--peers--peer_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--peers--peer_idoutput', 'GET_--network--peers--peer_id')">Output format</button>
    </div><div id="GET_--network--peers--peer_iddescr" class="GET_--network--peers--peer_id tabcontent">
            <p>
            Details about a given peer.</p>
            </div>
  <div id="GET_--network--peers--peer_idoutput" class="GET_--network--peers--peer_id tabcontent">
    <pre>
    { "score": number,
      "trusted": boolean,
      "state": "running" | "accepted" | "disconnected",
      "reachable_at"?: { "addr": string,
                         "port"?: integer ∈ [0, 2^16-1] },
      "stat":
        { "total_sent": integer ∈ [-2^31-2, 2^31+2] || string,
          "total_recv": integer ∈ [-2^31-2, 2^31+2] || string,
          "current_inflow": integer ∈ [-2^30-2, 2^30+2],
          "current_outflow": integer ∈ [-2^30-2, 2^30+2] },
      "last_failed_connection"?:
        [ { "addr": string,
            "port"?: integer ∈ [0, 2^16-1] },
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
      "last_rejected_connection"?:
        [ { "addr": string,
            "port"?: integer ∈ [0, 2^16-1] },
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
      "last_established_connection"?:
        [ { "addr": string,
            "port"?: integer ∈ [0, 2^16-1] },
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
      "last_disconnection"?:
        [ { "addr": string,
            "port"?: integer ∈ [0, 2^16-1] },
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
      "last_seen"?:
        [ { "addr": string,
            "port"?: integer ∈ [0, 2^16-1] },
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
      "last_miss"?:
        [ { "addr": string,
            "port"?: integer ∈ [0, 2^16-1] },
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ] }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_--network--peers--peer_id--ban :

**GET /network/peers/<peer_id>/ban**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--peers--peer_id--bandescr', 'GET_--network--peers--peer_id--ban')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--peers--peer_id--banoutput', 'GET_--network--peers--peer_id--ban')">Output format</button>
    </div><div id="GET_--network--peers--peer_id--bandescr" class="GET_--network--peers--peer_id--ban tabcontent">
            <p>
            Blacklist the given peer.</p>
            </div>
  <div id="GET_--network--peers--peer_id--banoutput" class="GET_--network--peers--peer_id--ban tabcontent">
    <pre>
    {  }</pre>
    </div>
  


.. _GET_--network--peers--peer_id--banned :

**GET /network/peers/<peer_id>/banned**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--peers--peer_id--banneddescr', 'GET_--network--peers--peer_id--banned')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--peers--peer_id--bannedoutput', 'GET_--network--peers--peer_id--banned')">Output format</button>
    </div><div id="GET_--network--peers--peer_id--banneddescr" class="GET_--network--peers--peer_id--banned tabcontent">
            <p>
            Check if a given peer is blacklisted or greylisted.</p>
            </div>
  <div id="GET_--network--peers--peer_id--bannedoutput" class="GET_--network--peers--peer_id--banned tabcontent">
    <pre>
    boolean</pre>
    </div>
  


.. _GET_--network--peers--peer_id--log :

**GET /network/peers/<peer_id>/log?[monitor]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--peers--peer_id--logdescr', 'GET_--network--peers--peer_id--log')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--peers--peer_id--logoutput', 'GET_--network--peers--peer_id--log')">Output format</button>
    </div><div id="GET_--network--peers--peer_id--logdescr" class="GET_--network--peers--peer_id--log tabcontent">
            <p>
            Monitor network events related to a given peer.</p> <p>Optional query arguments :<ul><li><span class="query">monitor</span></li></ul></p>
            </div>
  <div id="GET_--network--peers--peer_id--logoutput" class="GET_--network--peers--peer_id--log tabcontent">
    <pre>
    [ { "kind":
          "rejecting_request"
          | "incoming_request"
          | "disconnection"
          | "external_disconnection"
          | "connection_established"
          | "request_rejected",
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "addr": string,
        "port"?: integer ∈ [-2^15, 2^15-1] } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _GET_--network--peers--peer_id--trust :

**GET /network/peers/<peer_id>/trust**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--peers--peer_id--trustdescr', 'GET_--network--peers--peer_id--trust')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--peers--peer_id--trustoutput', 'GET_--network--peers--peer_id--trust')">Output format</button>
    </div><div id="GET_--network--peers--peer_id--trustdescr" class="GET_--network--peers--peer_id--trust tabcontent">
            <p>
            Trust a given peer permanently: the peer cannot be blocked (but its host IP still can).</p>
            </div>
  <div id="GET_--network--peers--peer_id--trustoutput" class="GET_--network--peers--peer_id--trust tabcontent">
    <pre>
    {  }</pre>
    </div>
  


.. _GET_--network--points :

**GET /network/points?(filter=<p2p.point.state_filter>)\***

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--pointsdescr', 'GET_--network--points')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--pointsoutput', 'GET_--network--points')">Output format</button>
    </div><div id="GET_--network--pointsdescr" class="GET_--network--points tabcontent">
            <p>
            List the pool of known `IP:port` used for establishing P2P connections.</p> <p>Optional query arguments :<ul><li><span class="query">filter = &lt;p2p.point.state_filter&gt;</span></li></ul></p>
            </div>
  <div id="GET_--network--pointsoutput" class="GET_--network--points tabcontent">
    <pre>
    [ [ string,
        { "trusted": boolean,
          "greylisted_until"?:
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "state":
            { "event_kind": "requested" }
            || { "event_kind": "accepted",
                 "p2p_peer_id":
                   string
                   /* A Cryptobox public key ID (Base58Check-encoded) */ }
            || { "event_kind": "running",
                 "p2p_peer_id":
                   string
                   /* A Cryptobox public key ID (Base58Check-encoded) */ }
            || { "event_kind": "disconnected" },
          "p2p_peer_id"?:
            string
            /* A Cryptobox public key ID (Base58Check-encoded) */,
          "last_failed_connection"?:
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "last_rejected_connection"?:
            [ string
              /* A Cryptobox public key ID (Base58Check-encoded) */,
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
          "last_established_connection"?:
            [ string
              /* A Cryptobox public key ID (Base58Check-encoded) */,
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
          "last_disconnection"?:
            [ string
              /* A Cryptobox public key ID (Base58Check-encoded) */,
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
          "last_seen"?:
            [ string
              /* A Cryptobox public key ID (Base58Check-encoded) */,
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
          "last_miss"?:
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string } ] ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_--network--points--point :

**GET /network/points/<point>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--points--pointdescr', 'GET_--network--points--point')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--points--pointoutput', 'GET_--network--points--point')">Output format</button>
    </div><div id="GET_--network--points--pointdescr" class="GET_--network--points--point tabcontent">
            <p>
            Details about a given `IP:addr`.</p>
            </div>
  <div id="GET_--network--points--pointoutput" class="GET_--network--points--point tabcontent">
    <pre>
    { "trusted": boolean,
      "greylisted_until"?:
        /* timestamp */
        $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
      "state":
        { "event_kind": "requested" }
        || { "event_kind": "accepted",
             "p2p_peer_id":
               string
               /* A Cryptobox public key ID (Base58Check-encoded) */ }
        || { "event_kind": "running",
             "p2p_peer_id":
               string
               /* A Cryptobox public key ID (Base58Check-encoded) */ }
        || { "event_kind": "disconnected" },
      "p2p_peer_id"?:
        string
        /* A Cryptobox public key ID (Base58Check-encoded) */,
      "last_failed_connection"?:
        /* timestamp */
        $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
      "last_rejected_connection"?:
        [ string
          /* A Cryptobox public key ID (Base58Check-encoded) */,
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
      "last_established_connection"?:
        [ string
          /* A Cryptobox public key ID (Base58Check-encoded) */,
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
      "last_disconnection"?:
        [ string
          /* A Cryptobox public key ID (Base58Check-encoded) */,
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
      "last_seen"?:
        [ string
          /* A Cryptobox public key ID (Base58Check-encoded) */,
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string ],
      "last_miss"?:
        /* timestamp */
        $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _PUT_--network--points--point :

**PUT /network/points/<point>?[timeout=<float>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'PUT_--network--points--pointdescr', 'PUT_--network--points--point')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'PUT_--network--points--pointinput', 'PUT_--network--points--point')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'PUT_--network--points--pointoutput', 'PUT_--network--points--point')">Output format</button>
    </div><div id="PUT_--network--points--pointdescr" class="PUT_--network--points--point tabcontent">
            <p>
            Connect to a peer</p> <p>Optional query arguments :<ul><li><span class="query">timeout = &lt;float&gt;</span></li></ul></p>
            </div>
  <div id="PUT_--network--points--pointinput" class="PUT_--network--points--point tabcontent">
    <pre>
    {  }</pre>
    </div>
  <div id="PUT_--network--points--pointoutput" class="PUT_--network--points--point tabcontent">
    <pre>
    {  }</pre>
    </div>
  

.. _GET_--network--points--point--ban :

**GET /network/points/<point>/ban**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--points--point--bandescr', 'GET_--network--points--point--ban')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--points--point--banoutput', 'GET_--network--points--point--ban')">Output format</button>
    </div><div id="GET_--network--points--point--bandescr" class="GET_--network--points--point--ban tabcontent">
            <p>
            Blacklist the given address.</p>
            </div>
  <div id="GET_--network--points--point--banoutput" class="GET_--network--points--point--ban tabcontent">
    <pre>
    {  }</pre>
    </div>
  


.. _GET_--network--points--point--banned :

**GET /network/points/<point>/banned**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--points--point--banneddescr', 'GET_--network--points--point--banned')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--points--point--bannedoutput', 'GET_--network--points--point--banned')">Output format</button>
    </div><div id="GET_--network--points--point--banneddescr" class="GET_--network--points--point--banned tabcontent">
            <p>
            Check is a given address is blacklisted or greylisted.</p>
            </div>
  <div id="GET_--network--points--point--bannedoutput" class="GET_--network--points--point--banned tabcontent">
    <pre>
    boolean</pre>
    </div>
  


.. _GET_--network--points--point--log :

**GET /network/points/<point>/log?[monitor]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--points--point--logdescr', 'GET_--network--points--point--log')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--points--point--logoutput', 'GET_--network--points--point--log')">Output format</button>
    </div><div id="GET_--network--points--point--logdescr" class="GET_--network--points--point--log tabcontent">
            <p>
            Monitor network events related to an `IP:addr`.</p> <p>Optional query arguments :<ul><li><span class="query">monitor</span></li></ul></p>
            </div>
  <div id="GET_--network--points--point--logoutput" class="GET_--network--points--point--log tabcontent">
    <pre>
    [ { "kind":
          { "event_kind": "outgoing_request" }
          || { "event_kind": "accepting_request",
               "p2p_peer_id":
                 string
                 /* A Cryptobox public key ID (Base58Check-encoded) */ }
          || { "event_kind": "rejecting_request",
               "p2p_peer_id":
                 string
                 /* A Cryptobox public key ID (Base58Check-encoded) */ }
          || { "event_kind": "request_rejected",
               "p2p_peer_id"?:
                 string
                 /* A Cryptobox public key ID (Base58Check-encoded) */ }
          || { "event_kind": "rejecting_request",
               "p2p_peer_id":
                 string
                 /* A Cryptobox public key ID (Base58Check-encoded) */ }
          || { "event_kind": "rejecting_request",
               "p2p_peer_id":
                 string
                 /* A Cryptobox public key ID (Base58Check-encoded) */ }
          || { "event_kind": "rejecting_request",
               "p2p_peer_id":
                 string
                 /* A Cryptobox public key ID (Base58Check-encoded) */ },
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _GET_--network--points--point--trust :

**GET /network/points/<point>/trust**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--points--point--trustdescr', 'GET_--network--points--point--trust')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--points--point--trustoutput', 'GET_--network--points--point--trust')">Output format</button>
    </div><div id="GET_--network--points--point--trustdescr" class="GET_--network--points--point--trust tabcontent">
            <p>
            Trust a given address permanently. Connections from this address can still be closed on authentication if the peer is blacklisted or greylisted.</p>
            </div>
  <div id="GET_--network--points--point--trustoutput" class="GET_--network--points--point--trust tabcontent">
    <pre>
    {  }</pre>
    </div>
  


.. _GET_--network--stat :

**GET /network/stat**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--statdescr', 'GET_--network--stat')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--statoutput', 'GET_--network--stat')">Output format</button>
    </div><div id="GET_--network--statdescr" class="GET_--network--stat tabcontent">
            <p>
            Global network bandwidth statistics in B/s.</p>
            </div>
  <div id="GET_--network--statoutput" class="GET_--network--stat tabcontent">
    <pre>
    { "total_sent": integer ∈ [-2^31-2, 2^31+2] || string,
      "total_recv": integer ∈ [-2^31-2, 2^31+2] || string,
      "current_inflow": integer ∈ [-2^30-2, 2^30+2],
      "current_outflow": integer ∈ [-2^30-2, 2^30+2] }</pre>
    </div>
  


.. _GET_--network--versions :

**GET /network/versions**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--network--versionsdescr', 'GET_--network--versions')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--network--versionsoutput', 'GET_--network--versions')">Output format</button>
    </div><div id="GET_--network--versionsdescr" class="GET_--network--versions tabcontent">
            <p>
            Supported network layer versions.</p>
            </div>
  <div id="GET_--network--versionsoutput" class="GET_--network--versions tabcontent">
    <pre>
    [ { "name": string,
        "major": integer ∈ [0, 2^16-1],
        "minor": integer ∈ [0, 2^16-1] } ... ]</pre>
    </div>
  


.. _GET_--protocols :

**GET /protocols**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--protocolsdescr', 'GET_--protocols')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--protocolsoutput', 'GET_--protocols')">Output format</button>
    </div><div id="GET_--protocolsdescr" class="GET_--protocols tabcontent">
            <p>
            </p>
            </div>
  <div id="GET_--protocolsoutput" class="GET_--protocols tabcontent">
    <pre>
    [ string
    /* A Tezos protocol ID (Base58Check-encoded) */ ... ]</pre>
    </div>
  

.. _GET_--protocols--Protocol_hash :

**GET /protocols/<Protocol_hash>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--protocols--Protocol_hashdescr', 'GET_--protocols--Protocol_hash')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--protocols--Protocol_hashoutput', 'GET_--protocols--Protocol_hash')">Output format</button>
    </div><div id="GET_--protocols--Protocol_hashdescr" class="GET_--protocols--Protocol_hash tabcontent">
            <p>
            </p>
            </div>
  <div id="GET_--protocols--Protocol_hashoutput" class="GET_--protocols--Protocol_hash tabcontent">
    <pre>
    { "expected_env_version": integer ∈ [-2^15, 2^15-1],
      "components":
        [ { "name": string,
            "interface"?: /^[a-zA-Z0-9]+$/,
            "implementation": /^[a-zA-Z0-9]+$/ } ... ] }</pre>
    </div>
  


.. _GET_--workers--block_validator :

**GET /workers/block_validator**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--workers--block_validatordescr', 'GET_--workers--block_validator')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--workers--block_validatoroutput', 'GET_--workers--block_validator')">Output format</button>
    </div><div id="GET_--workers--block_validatordescr" class="GET_--workers--block_validator tabcontent">
            <p>
            Introspect the state of the block_validator worker.</p>
            </div>
  <div id="GET_--workers--block_validatoroutput" class="GET_--workers--block_validator tabcontent">
    <pre>
    { "status":
        { "phase": "launching",
          "since":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "running",
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "closing",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "closed",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "crashed",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "errors":
               any
               /* The full list of error is available with the global RPC `GET
                  errors` */ },
      "pending_requests":
        [ { "pushed":
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
            "request":
              { "block": string /* A block identifier (Base58Check-encoded) */,
                "chain_id":
                  string
                  /* Network identifier (Base58Check-encoded) */,
                "peer"?:
                  string
                  /* A Cryptobox public key ID (Base58Check-encoded) */ } } ... ],
      "backlog":
        [ { "level": string,
            "events":
              [ /* Event state */
              { "message": string }
              || { "successful_validation":
                     { "block":
                         string
                         /* A block identifier (Base58Check-encoded) */,
                       "chain_id":
                         string
                         /* Network identifier (Base58Check-encoded) */,
                       "peer"?:
                         string
                         /* A Cryptobox public key ID (Base58Check-encoded) */ },
                   "status":
                     { "pushed":
                         /* timestamp */
                         $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
                       "treated":
                         /* timestamp */
                         $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
                       "completed":
                         /* timestamp */
                         $timestamp || integer ∈ [-2^31-2, 2^31+2] || string } }
              || { "failed_validation":
                     { "block":
                         string
                         /* A block identifier (Base58Check-encoded) */,
                       "chain_id":
                         string
                         /* Network identifier (Base58Check-encoded) */,
                       "peer"?:
                         string
                         /* A Cryptobox public key ID (Base58Check-encoded) */ },
                   "status":
                     { "pushed":
                         /* timestamp */
                         $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
                       "treated":
                         /* timestamp */
                         $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
                       "completed":
                         /* timestamp */
                         $timestamp || integer ∈ [-2^31-2, 2^31+2] || string },
                   "errors"?:
                     any
                     /* The full list of error is available with the global RPC
                        `GET errors` */ } ... ] } ... ],
      "current_request"?:
        { "pushed":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "treated":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "request":
            { "block": string /* A block identifier (Base58Check-encoded) */,
              "chain_id": string /* Network identifier (Base58Check-encoded) */,
              "peer"?:
                string
                /* A Cryptobox public key ID (Base58Check-encoded) */ } } }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _GET_--workers--chain_validators :

**GET /workers/chain_validators**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--workers--chain_validatorsdescr', 'GET_--workers--chain_validators')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--workers--chain_validatorsoutput', 'GET_--workers--chain_validators')">Output format</button>
    </div><div id="GET_--workers--chain_validatorsdescr" class="GET_--workers--chain_validators tabcontent">
            <p>
            Lists the chain validator workers and their status.</p>
            </div>
  <div id="GET_--workers--chain_validatorsoutput" class="GET_--workers--chain_validators tabcontent">
    <pre>
    [ { "chain_id": string /* Network identifier (Base58Check-encoded) */,
        "status":
          { "phase": "launching",
            "since":
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "running",
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "closing",
               "birth":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "closed",
               "birth":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "crashed",
               "birth":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "errors":
                 any
                 /* The full list of error is available with the global RPC
                    `GET errors` */ } } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_--workers--chain_validators--chain_id :

**GET /workers/chain_validators/<chain_id>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--workers--chain_validators--chain_iddescr', 'GET_--workers--chain_validators--chain_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--workers--chain_validators--chain_idoutput', 'GET_--workers--chain_validators--chain_id')">Output format</button>
    </div><div id="GET_--workers--chain_validators--chain_iddescr" class="GET_--workers--chain_validators--chain_id tabcontent">
            <p>
            Introspect the state of a chain validator worker.</p>
            </div>
  <div id="GET_--workers--chain_validators--chain_idoutput" class="GET_--workers--chain_validators--chain_id tabcontent">
    <pre>
    { "status":
        { "phase": "launching",
          "since":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "running",
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "closing",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "closed",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "crashed",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "errors":
               any
               /* The full list of error is available with the global RPC `GET
                  errors` */ },
      "pending_requests":
        [ { "pushed":
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
            "request": string /* A block identifier (Base58Check-encoded) */ } ... ],
      "backlog":
        [ { "level": string,
            "events":
              [ { "request":
                    string
                    /* A block identifier (Base58Check-encoded) */,
                  "status":
                    { "pushed":
                        /* timestamp */
                        $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
                      "treated":
                        /* timestamp */
                        $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
                      "completed":
                        /* timestamp */
                        $timestamp || integer ∈ [-2^31-2, 2^31+2] || string },
                  "outcome": "branch" | "ignored" | "increment",
                  "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */ }
                || any
                /* The full list of error is available with the global RPC `GET
                   errors` */ ... ] } ... ],
      "current_request"?:
        { "pushed":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "treated":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "request": string /* A block identifier (Base58Check-encoded) */ } }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_--workers--chain_validators--chain_id--peers_validators :

**GET /workers/chain_validators/<chain_id>/peers_validators**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--workers--chain_validators--chain_id--peers_validatorsdescr', 'GET_--workers--chain_validators--chain_id--peers_validators')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--workers--chain_validators--chain_id--peers_validatorsoutput', 'GET_--workers--chain_validators--chain_id--peers_validators')">Output format</button>
    </div><div id="GET_--workers--chain_validators--chain_id--peers_validatorsdescr" class="GET_--workers--chain_validators--chain_id--peers_validators tabcontent">
            <p>
            Lists the peer validator workers and their status.</p>
            </div>
  <div id="GET_--workers--chain_validators--chain_id--peers_validatorsoutput" class="GET_--workers--chain_validators--chain_id--peers_validators tabcontent">
    <pre>
    [ { "peer_id": string /* A Cryptobox public key ID (Base58Check-encoded) */,
        "status":
          { "phase": "launching",
            "since":
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "running",
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "closing",
               "birth":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "closed",
               "birth":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "crashed",
               "birth":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "errors":
                 any
                 /* The full list of error is available with the global RPC
                    `GET errors` */ } } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_--workers--chain_validators--chain_id--peers_validators--peer_id :

**GET /workers/chain_validators/<chain_id>/peers_validators/<peer_id>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--workers--chain_validators--chain_id--peers_validators--peer_iddescr', 'GET_--workers--chain_validators--chain_id--peers_validators--peer_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--workers--chain_validators--chain_id--peers_validators--peer_idoutput', 'GET_--workers--chain_validators--chain_id--peers_validators--peer_id')">Output format</button>
    </div><div id="GET_--workers--chain_validators--chain_id--peers_validators--peer_iddescr" class="GET_--workers--chain_validators--chain_id--peers_validators--peer_id tabcontent">
            <p>
            Introspect the state of a peer validator worker.</p>
            </div>
  <div id="GET_--workers--chain_validators--chain_id--peers_validators--peer_idoutput" class="GET_--workers--chain_validators--chain_id--peers_validators--peer_id tabcontent">
    <pre>
    { "status":
        { "phase": "launching",
          "since":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "running",
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "closing",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "closed",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "crashed",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "errors":
               any
               /* The full list of error is available with the global RPC `GET
                  errors` */ },
      "pending_requests":
        [ { "pushed":
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
            "request":
              { "request": "new_head",
                "block": string /* A block identifier (Base58Check-encoded) */ }
              || { "request": "new_branch",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "locator_length": integer ∈ [0, 2^16-1] } } ... ],
      "backlog":
        [ { "level": string,
            "events":
              [ { "message": string }
                || { "request":
                       { "request": "new_head",
                         "block":
                           string
                           /* A block identifier (Base58Check-encoded) */ }
                       || { "request": "new_branch",
                            "block":
                              string
                              /* A block identifier (Base58Check-encoded) */,
                            "locator_length": integer ∈ [0, 2^16-1] },
                     "status":
                       { "pushed":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string,
                         "treated":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string,
                         "completed":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string } }
                || { "error":
                       any
                       /* The full list of error is available with the global
                          RPC `GET errors` */,
                     "failed_request":
                       { "request": "new_head",
                         "block":
                           string
                           /* A block identifier (Base58Check-encoded) */ }
                       || { "request": "new_branch",
                            "block":
                              string
                              /* A block identifier (Base58Check-encoded) */,
                            "locator_length": integer ∈ [0, 2^16-1] },
                     "status":
                       { "pushed":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string,
                         "treated":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string,
                         "completed":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string } } ... ] } ... ],
      "current_request"?:
        { "pushed":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "treated":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "request":
            { "request": "new_head",
              "block": string /* A block identifier (Base58Check-encoded) */ }
            || { "request": "new_branch",
                 "block": string /* A block identifier (Base58Check-encoded) */,
                 "locator_length": integer ∈ [0, 2^16-1] } } }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _GET_--workers--prevalidators :

**GET /workers/prevalidators**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--workers--prevalidatorsdescr', 'GET_--workers--prevalidators')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--workers--prevalidatorsoutput', 'GET_--workers--prevalidators')">Output format</button>
    </div><div id="GET_--workers--prevalidatorsdescr" class="GET_--workers--prevalidators tabcontent">
            <p>
            Lists the Prevalidator workers and their status.</p>
            </div>
  <div id="GET_--workers--prevalidatorsoutput" class="GET_--workers--prevalidators tabcontent">
    <pre>
    [ { "chain_id": string /* Network identifier (Base58Check-encoded) */,
        "status":
          { "phase": "launching",
            "since":
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "running",
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "closing",
               "birth":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "closed",
               "birth":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "phase": "crashed",
               "birth":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "since":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
               "errors":
                 any
                 /* The full list of error is available with the global RPC
                    `GET errors` */ } } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_--workers--prevalidators--chain_id :

**GET /workers/prevalidators/<chain_id>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_--workers--prevalidators--chain_iddescr', 'GET_--workers--prevalidators--chain_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_--workers--prevalidators--chain_idoutput', 'GET_--workers--prevalidators--chain_id')">Output format</button>
    </div><div id="GET_--workers--prevalidators--chain_iddescr" class="GET_--workers--prevalidators--chain_id tabcontent">
            <p>
            Introspect the state of a prevalidator worker.</p>
            </div>
  <div id="GET_--workers--prevalidators--chain_idoutput" class="GET_--workers--prevalidators--chain_id tabcontent">
    <pre>
    { "status":
        { "phase": "launching",
          "since":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "running",
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "closing",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "closed",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
        || { "phase": "crashed",
             "birth":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "since":
               /* timestamp */
               $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
             "errors":
               any
               /* The full list of error is available with the global RPC `GET
                  errors` */ },
      "pending_requests":
        [ { "pushed":
              /* timestamp */
              $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
            "request":
              { "request": "flush",
                "block": string /* A block identifier (Base58Check-encoded) */ }
              || { "request": "notify",
                   "peer":
                     string
                     /* A Cryptobox public key ID (Base58Check-encoded) */,
                   "mempool":
                     { "known_valid":
                         [ string
                         /* A Tezos operation ID (Base58Check-encoded) */ ... ],
                       "pending":
                         [ string
                         /* A Tezos operation ID (Base58Check-encoded) */ ... ] } }
              || { "request": "inject",
                   "operation":
                     { "branch":
                         string
                         /* A block identifier (Base58Check-encoded) */,
                       "data": /^[a-zA-Z0-9]+$/ } }
              || { "request": "arrived",
                   "operation_hash":
                     string
                     /* A Tezos operation ID (Base58Check-encoded) */,
                   "operation":
                     { "branch":
                         string
                         /* A block identifier (Base58Check-encoded) */,
                       "data": /^[a-zA-Z0-9]+$/ } }
              || { "request": "advertise" } } ... ],
      "backlog":
        [ { "level": string,
            "events":
              [ { "message": string }
                || { "request":
                       { "request": "flush",
                         "block":
                           string
                           /* A block identifier (Base58Check-encoded) */ }
                       || { "request": "notify",
                            "peer":
                              string
                              /* A Cryptobox public key ID
                                 (Base58Check-encoded) */,
                            "mempool":
                              { "known_valid":
                                  [ string
                                  /* A Tezos operation ID (Base58Check-encoded) */ ... ],
                                "pending":
                                  [ string
                                  /* A Tezos operation ID (Base58Check-encoded) */ ... ] } }
                       || { "request": "inject",
                            "operation":
                              { "branch":
                                  string
                                  /* A block identifier (Base58Check-encoded) */,
                                "data": /^[a-zA-Z0-9]+$/ } }
                       || { "request": "arrived",
                            "operation_hash":
                              string
                              /* A Tezos operation ID (Base58Check-encoded) */,
                            "operation":
                              { "branch":
                                  string
                                  /* A block identifier (Base58Check-encoded) */,
                                "data": /^[a-zA-Z0-9]+$/ } }
                       || { "request": "advertise" },
                     "status":
                       { "pushed":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string,
                         "treated":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string,
                         "completed":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string } }
                || { "error":
                       any
                       /* The full list of error is available with the global
                          RPC `GET errors` */,
                     "failed_request":
                       { "request": "flush",
                         "block":
                           string
                           /* A block identifier (Base58Check-encoded) */ }
                       || { "request": "notify",
                            "peer":
                              string
                              /* A Cryptobox public key ID
                                 (Base58Check-encoded) */,
                            "mempool":
                              { "known_valid":
                                  [ string
                                  /* A Tezos operation ID (Base58Check-encoded) */ ... ],
                                "pending":
                                  [ string
                                  /* A Tezos operation ID (Base58Check-encoded) */ ... ] } }
                       || { "request": "inject",
                            "operation":
                              { "branch":
                                  string
                                  /* A block identifier (Base58Check-encoded) */,
                                "data": /^[a-zA-Z0-9]+$/ } }
                       || { "request": "arrived",
                            "operation_hash":
                              string
                              /* A Tezos operation ID (Base58Check-encoded) */,
                            "operation":
                              { "branch":
                                  string
                                  /* A block identifier (Base58Check-encoded) */,
                                "data": /^[a-zA-Z0-9]+$/ } }
                       || { "request": "advertise" },
                     "status":
                       { "pushed":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string,
                         "treated":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string,
                         "completed":
                           /* timestamp */
                           $timestamp
                           || integer ∈ [-2^31-2, 2^31+2] || string } } ... ] } ... ],
      "current_request"?:
        { "pushed":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "treated":
            /* timestamp */
            $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
          "request":
            { "request": "flush",
              "block": string /* A block identifier (Base58Check-encoded) */ }
            || { "request": "notify",
                 "peer":
                   string
                   /* A Cryptobox public key ID (Base58Check-encoded) */,
                 "mempool":
                   { "known_valid":
                       [ string
                       /* A Tezos operation ID (Base58Check-encoded) */ ... ],
                     "pending":
                       [ string
                       /* A Tezos operation ID (Base58Check-encoded) */ ... ] } }
            || { "request": "inject",
                 "operation":
                   { "branch":
                       string
                       /* A block identifier (Base58Check-encoded) */,
                     "data": /^[a-zA-Z0-9]+$/ } }
            || { "request": "arrived",
                 "operation_hash":
                   string
                   /* A Tezos operation ID (Base58Check-encoded) */,
                 "operation":
                   { "branch":
                       string
                       /* A block identifier (Base58Check-encoded) */,
                     "data": /^[a-zA-Z0-9]+$/ } }
            || { "request": "advertise" } } }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  



Protocol Alpha
==============

.. _GET_..--block_id :

**GET ../<block_id>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_iddescr', 'GET_..--block_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_idoutput', 'GET_..--block_id')">Output format</button>
    </div><div id="GET_..--block_iddescr" class="GET_..--block_id tabcontent">
            <p>
            All the information about a block.</p>
            </div>
  <div id="GET_..--block_idoutput" class="GET_..--block_id tabcontent">
    <pre>
    { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
      "chain_id": string /* Network identifier (Base58Check-encoded) */,
      "hash": string /* A block identifier (Base58Check-encoded) */,
      "header": $raw_block_header,
      "metadata": $block_header_metadata,
      "operations": [ [ $operation ... ] ... ] }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_header.alpha.full_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $block_header_metadata:
      { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "next_protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "test_chain_status":
          /* Test chain status */
          { "status": "not_running" }
          || { "status": "forking",
               "protocol":
                 string
                 /* A Tezos protocol ID (Base58Check-encoded) */,
               "expiration":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "status": "running",
               "chain_id":
                 string
                 /* Network identifier (Base58Check-encoded) */,
               "genesis": string /* A block identifier (Base58Check-encoded) */,
               "protocol":
                 string
                 /* A Tezos protocol ID (Base58Check-encoded) */,
               "expiration":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string },
        "max_operations_ttl": integer ∈ [-2^30-2, 2^30+2],
        "max_operation_data_length": integer ∈ [-2^30-2, 2^30+2],
        "max_block_header_length": integer ∈ [-2^30-2, 2^30+2],
        "max_operation_list_length":
          [ { "max_size": integer ∈ [-2^30-2, 2^30+2],
              "max_op"?: integer ∈ [-2^30-2, 2^30+2] } ... ],
        "baker":
          string
          /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
        "level":
          { "level": integer ∈ [-2^31-2, 2^31+2],
            "level_position": integer ∈ [-2^31-2, 2^31+2],
            "cycle": integer ∈ [-2^31-2, 2^31+2],
            "cycle_position": integer ∈ [-2^31-2, 2^31+2],
            "voting_period": integer ∈ [-2^31-2, 2^31+2],
            "voting_period_position": integer ∈ [-2^31-2, 2^31+2],
            "expected_commitment": boolean },
        "voting_period_kind":
          "proposal" || "testing_vote" || "testing" || "promotion_vote" }
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $error:
      /* ... FIXME ... */
      any
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $operation:
      { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "chain_id": string /* Network identifier (Base58Check-encoded) */,
        "hash": string /* A Tezos operation ID (Base58Check-encoded) */,
        "branch": string /* A block identifier (Base58Check-encoded) */,
        "contents": [ $operation.alpha.operation_contents_and_result ... ],
        "signature"?:
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $operation.alpha.internal_operation_result:
      { "kind": "reveal",
        "source": $contract_id,
        "nonce": integer ∈ [0, 2^16-1],
        "public_key":
          string
          /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
        "result": $operation.alpha.operation_result.reveal }
      || { "kind": "transaction",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "result": $operation.alpha.operation_result.transaction }
      || { "kind": "origination",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "result": $operation.alpha.operation_result.origination }
      || { "kind": "delegation",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "result": $operation.alpha.operation_result.delegation }
    $operation.alpha.operation_contents_and_result:
      { "kind": "endorsement",
        "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ],
        "metadata":
          { "delegate":
              string
              /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
            "slots": [ integer ∈ [0, 255] ... ] } }
      || { "kind": "seed_nonce_revelation",
           "level": integer ∈ [-2^31-2, 2^31+2],
           "nonce": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_endorsement_evidence",
           "op1":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "op2":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_baking_evidence",
           "bh1": $block_header.alpha.full_header,
           "bh2": $block_header.alpha.full_header,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "activate_account",
           "pkh": string /* An Ed25519 public key hash (Base58Check-encoded) */,
           "secret": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "proposals",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposals":
             [ string
             /* A Tezos protocol ID (Base58Check-encoded) */ ... ],
           "metadata": {  } }
      || { "kind": "ballot",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposal": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "ballot": "nay" | "yay" | "pass",
           "metadata": {  } }
      || { "kind": "reveal",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "public_key":
             string
             /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.reveal,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "transaction",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.transaction,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "origination",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.origination,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "delegation",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.delegation,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "activate_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
      || { "kind": "activate_test_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
    $operation.alpha.operation_result.delegation:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.origination:
      { "status": "applied",
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.reveal:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.transaction:
      { "status": "applied",
        "storage"?: $micheline.michelson_v1.expression,
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation_metadata.alpha.balance_updates:
      [ { "kind": "contract",
          "contract": $contract_id,
          "credited":
            /* Amount in mutez */
            integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "contract",
             "contract": $contract_id,
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string } ... ]
    $raw_block_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_..--block_id--context--constants :

**GET ../<block_id>/context/constants**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constantsdescr', 'GET_..--block_id--context--constants')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constantsoutput', 'GET_..--block_id--context--constants')">Output format</button>
    </div><div id="GET_..--block_id--context--constantsdescr" class="GET_..--block_id--context--constants tabcontent">
            <p>
            All constants</p>
            </div>
  <div id="GET_..--block_id--context--constantsoutput" class="GET_..--block_id--context--constants tabcontent">
    <pre>
    { "proof_of_work_nonce_size": integer ∈ [0, 255],
      "nonce_length": integer ∈ [0, 255],
      "max_revelations_per_block": integer ∈ [0, 255],
      "preserved_cycles": integer ∈ [0, 255],
      "blocks_per_cycle": integer ∈ [-2^31-2, 2^31+2],
      "blocks_per_commitment": integer ∈ [-2^31-2, 2^31+2],
      "blocks_per_roll_snapshot": integer ∈ [-2^31-2, 2^31+2],
      "blocks_per_voting_period": integer ∈ [-2^31-2, 2^31+2],
      "time_between_blocks": [ integer ∈ [-2^31-2, 2^31+2] || string ... ],
      "first_free_baking_slot": integer ∈ [0, 2^16-1],
      "endorsers_per_block": integer ∈ [0, 2^16-1],
      "hard_gas_limit_per_operation": $bignum,
      "hard_gas_limit_per_block": $bignum,
      "proof_of_work_threshold": integer ∈ [-2^31-2, 2^31+2] || string,
      "max_operation_data_length": integer ∈ [-2^30-2, 2^30+2],
      "tokens_per_roll":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "michelson_maximum_type_size": integer ∈ [0, 2^16-1],
      "seed_nonce_revelation_tip":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "origination_burn":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "block_security_deposit":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "endorsement_security_deposit":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "block_reward":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "endorsement_reward":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "cost_per_byte":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "hard_storage_limit_per_operation":
        integer ∈ [-2^31-2, 2^31+2] || string,
      "hard_storage_limit_per_block": integer ∈ [-2^31-2, 2^31+2] || string }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string</pre>
    </div>
  

.. _GET_..--block_id--context--constants--block_reward :

**GET ../<block_id>/context/constants/block_reward**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--block_rewarddescr', 'GET_..--block_id--context--constants--block_reward')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--block_rewardoutput', 'GET_..--block_id--context--constants--block_reward')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--block_rewarddescr" class="GET_..--block_id--context--constants--block_reward tabcontent">
            <p>
            block_reward</p>
            </div>
  <div id="GET_..--block_id--context--constants--block_rewardoutput" class="GET_..--block_id--context--constants--block_reward tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--constants--block_security_deposit :

**GET ../<block_id>/context/constants/block_security_deposit**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--block_security_depositdescr', 'GET_..--block_id--context--constants--block_security_deposit')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--block_security_depositoutput', 'GET_..--block_id--context--constants--block_security_deposit')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--block_security_depositdescr" class="GET_..--block_id--context--constants--block_security_deposit tabcontent">
            <p>
            block_security_deposit</p>
            </div>
  <div id="GET_..--block_id--context--constants--block_security_depositoutput" class="GET_..--block_id--context--constants--block_security_deposit tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--constants--blocks_per_commitment :

**GET ../<block_id>/context/constants/blocks_per_commitment**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--blocks_per_commitmentdescr', 'GET_..--block_id--context--constants--blocks_per_commitment')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--blocks_per_commitmentoutput', 'GET_..--block_id--context--constants--blocks_per_commitment')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--blocks_per_commitmentdescr" class="GET_..--block_id--context--constants--blocks_per_commitment tabcontent">
            <p>
            How many blocks between random seed's nonce commitment</p>
            </div>
  <div id="GET_..--block_id--context--constants--blocks_per_commitmentoutput" class="GET_..--block_id--context--constants--blocks_per_commitment tabcontent">
    <pre>
    integer ∈ [-2^31-2, 2^31+2]</pre>
    </div>
  


.. _GET_..--block_id--context--constants--blocks_per_cycle :

**GET ../<block_id>/context/constants/blocks_per_cycle**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--blocks_per_cycledescr', 'GET_..--block_id--context--constants--blocks_per_cycle')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--blocks_per_cycleoutput', 'GET_..--block_id--context--constants--blocks_per_cycle')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--blocks_per_cycledescr" class="GET_..--block_id--context--constants--blocks_per_cycle tabcontent">
            <p>
            Cycle length</p>
            </div>
  <div id="GET_..--block_id--context--constants--blocks_per_cycleoutput" class="GET_..--block_id--context--constants--blocks_per_cycle tabcontent">
    <pre>
    integer ∈ [-2^31-2, 2^31+2]</pre>
    </div>
  


.. _GET_..--block_id--context--constants--blocks_per_roll_snapshot :

**GET ../<block_id>/context/constants/blocks_per_roll_snapshot**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--blocks_per_roll_snapshotdescr', 'GET_..--block_id--context--constants--blocks_per_roll_snapshot')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--blocks_per_roll_snapshotoutput', 'GET_..--block_id--context--constants--blocks_per_roll_snapshot')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--blocks_per_roll_snapshotdescr" class="GET_..--block_id--context--constants--blocks_per_roll_snapshot tabcontent">
            <p>
            How many blocks between roll snapshots</p>
            </div>
  <div id="GET_..--block_id--context--constants--blocks_per_roll_snapshotoutput" class="GET_..--block_id--context--constants--blocks_per_roll_snapshot tabcontent">
    <pre>
    integer ∈ [-2^31-2, 2^31+2]</pre>
    </div>
  


.. _GET_..--block_id--context--constants--blocks_per_voting_period :

**GET ../<block_id>/context/constants/blocks_per_voting_period**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--blocks_per_voting_perioddescr', 'GET_..--block_id--context--constants--blocks_per_voting_period')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--blocks_per_voting_periodoutput', 'GET_..--block_id--context--constants--blocks_per_voting_period')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--blocks_per_voting_perioddescr" class="GET_..--block_id--context--constants--blocks_per_voting_period tabcontent">
            <p>
            Length of the voting period</p>
            </div>
  <div id="GET_..--block_id--context--constants--blocks_per_voting_periodoutput" class="GET_..--block_id--context--constants--blocks_per_voting_period tabcontent">
    <pre>
    integer ∈ [-2^31-2, 2^31+2]</pre>
    </div>
  


.. _GET_..--block_id--context--constants--cost_per_byte :

**GET ../<block_id>/context/constants/cost_per_byte**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--cost_per_bytedescr', 'GET_..--block_id--context--constants--cost_per_byte')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--cost_per_byteoutput', 'GET_..--block_id--context--constants--cost_per_byte')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--cost_per_bytedescr" class="GET_..--block_id--context--constants--cost_per_byte tabcontent">
            <p>
            The cost per bytes added to the storage</p>
            </div>
  <div id="GET_..--block_id--context--constants--cost_per_byteoutput" class="GET_..--block_id--context--constants--cost_per_byte tabcontent">
    <pre>
    { "cost_per_byte":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string }</pre>
    </div>
  


.. _GET_..--block_id--context--constants--endorsement_reward :

**GET ../<block_id>/context/constants/endorsement_reward**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--endorsement_rewarddescr', 'GET_..--block_id--context--constants--endorsement_reward')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--endorsement_rewardoutput', 'GET_..--block_id--context--constants--endorsement_reward')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--endorsement_rewarddescr" class="GET_..--block_id--context--constants--endorsement_reward tabcontent">
            <p>
            endorsement_reward</p>
            </div>
  <div id="GET_..--block_id--context--constants--endorsement_rewardoutput" class="GET_..--block_id--context--constants--endorsement_reward tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--constants--endorsement_security_deposit :

**GET ../<block_id>/context/constants/endorsement_security_deposit**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--endorsement_security_depositdescr', 'GET_..--block_id--context--constants--endorsement_security_deposit')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--endorsement_security_depositoutput', 'GET_..--block_id--context--constants--endorsement_security_deposit')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--endorsement_security_depositdescr" class="GET_..--block_id--context--constants--endorsement_security_deposit tabcontent">
            <p>
            endorsement_security_deposit</p>
            </div>
  <div id="GET_..--block_id--context--constants--endorsement_security_depositoutput" class="GET_..--block_id--context--constants--endorsement_security_deposit tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--constants--endorsers_per_block :

**GET ../<block_id>/context/constants/endorsers_per_block**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--endorsers_per_blockdescr', 'GET_..--block_id--context--constants--endorsers_per_block')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--endorsers_per_blockoutput', 'GET_..--block_id--context--constants--endorsers_per_block')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--endorsers_per_blockdescr" class="GET_..--block_id--context--constants--endorsers_per_block tabcontent">
            <p>
            Max signing slot</p>
            </div>
  <div id="GET_..--block_id--context--constants--endorsers_per_blockoutput" class="GET_..--block_id--context--constants--endorsers_per_block tabcontent">
    <pre>
    integer ∈ [0, 2^16-1]</pre>
    </div>
  


.. _GET_..--block_id--context--constants--errors :

**GET ../<block_id>/context/constants/errors**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--errorsdescr', 'GET_..--block_id--context--constants--errors')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--errorsoutput', 'GET_..--block_id--context--constants--errors')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--errorsdescr" class="GET_..--block_id--context--constants--errors tabcontent">
            <p>
            Schema for all the RPC errors from this protocol version</p>
            </div>
  <div id="GET_..--block_id--context--constants--errorsoutput" class="GET_..--block_id--context--constants--errors tabcontent">
    <pre>
    any</pre>
    </div>
  


.. _GET_..--block_id--context--constants--first_free_baking_slot :

**GET ../<block_id>/context/constants/first_free_baking_slot**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--first_free_baking_slotdescr', 'GET_..--block_id--context--constants--first_free_baking_slot')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--first_free_baking_slotoutput', 'GET_..--block_id--context--constants--first_free_baking_slot')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--first_free_baking_slotdescr" class="GET_..--block_id--context--constants--first_free_baking_slot tabcontent">
            <p>
            First free baking slot</p>
            </div>
  <div id="GET_..--block_id--context--constants--first_free_baking_slotoutput" class="GET_..--block_id--context--constants--first_free_baking_slot tabcontent">
    <pre>
    integer ∈ [0, 2^16-1]</pre>
    </div>
  


.. _GET_..--block_id--context--constants--hard_gas_limits :

**GET ../<block_id>/context/constants/hard_gas_limits**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--hard_gas_limitsdescr', 'GET_..--block_id--context--constants--hard_gas_limits')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--hard_gas_limitsoutput', 'GET_..--block_id--context--constants--hard_gas_limits')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--hard_gas_limitsdescr" class="GET_..--block_id--context--constants--hard_gas_limits tabcontent">
            <p>
            Hard maximum amount of gas per operation and per block</p>
            </div>
  <div id="GET_..--block_id--context--constants--hard_gas_limitsoutput" class="GET_..--block_id--context--constants--hard_gas_limits tabcontent">
    <pre>
    { "per_block": $bignum,
      "per_operation": $bignum }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string</pre>
    </div>
  


.. _GET_..--block_id--context--constants--hard_storage_limits :

**GET ../<block_id>/context/constants/hard_storage_limits**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--hard_storage_limitsdescr', 'GET_..--block_id--context--constants--hard_storage_limits')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--hard_storage_limitsoutput', 'GET_..--block_id--context--constants--hard_storage_limits')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--hard_storage_limitsdescr" class="GET_..--block_id--context--constants--hard_storage_limits tabcontent">
            <p>
            Hard maximum amount of bytes stored per operation and per block</p>
            </div>
  <div id="GET_..--block_id--context--constants--hard_storage_limitsoutput" class="GET_..--block_id--context--constants--hard_storage_limits tabcontent">
    <pre>
    { "per_block": integer ∈ [-2^31-2, 2^31+2] || string,
      "per_operation": integer ∈ [-2^31-2, 2^31+2] || string }</pre>
    </div>
  


.. _GET_..--block_id--context--constants--origination_burn :

**GET ../<block_id>/context/constants/origination_burn**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--origination_burndescr', 'GET_..--block_id--context--constants--origination_burn')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--origination_burnoutput', 'GET_..--block_id--context--constants--origination_burn')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--origination_burndescr" class="GET_..--block_id--context--constants--origination_burn tabcontent">
            <p>
            origination_burn</p>
            </div>
  <div id="GET_..--block_id--context--constants--origination_burnoutput" class="GET_..--block_id--context--constants--origination_burn tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--constants--preserved_cycles :

**GET ../<block_id>/context/constants/preserved_cycles**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--preserved_cyclesdescr', 'GET_..--block_id--context--constants--preserved_cycles')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--preserved_cyclesoutput', 'GET_..--block_id--context--constants--preserved_cycles')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--preserved_cyclesdescr" class="GET_..--block_id--context--constants--preserved_cycles tabcontent">
            <p>
            How many cycle before the 'no-automatic-fork point'</p>
            </div>
  <div id="GET_..--block_id--context--constants--preserved_cyclesoutput" class="GET_..--block_id--context--constants--preserved_cycles tabcontent">
    <pre>
    integer ∈ [-2^30-2, 2^30+2]</pre>
    </div>
  


.. _GET_..--block_id--context--constants--proof_of_work_threshold :

**GET ../<block_id>/context/constants/proof_of_work_threshold**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--proof_of_work_thresholddescr', 'GET_..--block_id--context--constants--proof_of_work_threshold')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--proof_of_work_thresholdoutput', 'GET_..--block_id--context--constants--proof_of_work_threshold')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--proof_of_work_thresholddescr" class="GET_..--block_id--context--constants--proof_of_work_threshold tabcontent">
            <p>
            Stamp threshold</p>
            </div>
  <div id="GET_..--block_id--context--constants--proof_of_work_thresholdoutput" class="GET_..--block_id--context--constants--proof_of_work_threshold tabcontent">
    <pre>
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--constants--seed_nonce_revelation_tip :

**GET ../<block_id>/context/constants/seed_nonce_revelation_tip**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--seed_nonce_revelation_tipdescr', 'GET_..--block_id--context--constants--seed_nonce_revelation_tip')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--seed_nonce_revelation_tipoutput', 'GET_..--block_id--context--constants--seed_nonce_revelation_tip')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--seed_nonce_revelation_tipdescr" class="GET_..--block_id--context--constants--seed_nonce_revelation_tip tabcontent">
            <p>
            seed_nonce_revelation_tip</p>
            </div>
  <div id="GET_..--block_id--context--constants--seed_nonce_revelation_tipoutput" class="GET_..--block_id--context--constants--seed_nonce_revelation_tip tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--constants--time_between_slots :

**GET ../<block_id>/context/constants/time_between_slots**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--constants--time_between_slotsdescr', 'GET_..--block_id--context--constants--time_between_slots')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--constants--time_between_slotsoutput', 'GET_..--block_id--context--constants--time_between_slots')">Output format</button>
    </div><div id="GET_..--block_id--context--constants--time_between_slotsdescr" class="GET_..--block_id--context--constants--time_between_slots tabcontent">
            <p>
            Slot durations</p>
            </div>
  <div id="GET_..--block_id--context--constants--time_between_slotsoutput" class="GET_..--block_id--context--constants--time_between_slots tabcontent">
    <pre>
    [ integer ∈ [-2^31-2, 2^31+2] || string ... ]</pre>
    </div>
  


.. _GET_..--block_id--context--contracts :

**GET ../<block_id>/context/contracts**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contractsdescr', 'GET_..--block_id--context--contracts')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contractsoutput', 'GET_..--block_id--context--contracts')">Output format</button>
    </div><div id="GET_..--block_id--context--contractsdescr" class="GET_..--block_id--context--contracts tabcontent">
            <p>
            All existing contracts (including non-empty default contracts).</p>
            </div>
  <div id="GET_..--block_id--context--contractsoutput" class="GET_..--block_id--context--contracts tabcontent">
    <pre>
    [ $contract_id ... ]
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string</pre>
    </div>
  

.. _GET_..--block_id--context--contracts--contract_id :

**GET ../<block_id>/context/contracts/<contract_id>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_iddescr', 'GET_..--block_id--context--contracts--contract_id')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_idoutput', 'GET_..--block_id--context--contracts--contract_id')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_iddescr" class="GET_..--block_id--context--contracts--contract_id tabcontent">
            <p>
            Access the complete status of a contract.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_idoutput" class="GET_..--block_id--context--contracts--contract_id tabcontent">
    <pre>
    { "manager":
        string
        /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
      "balance": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
      "spendable": boolean,
      "delegate":
        { "setable": boolean,
          "value"?:
            string
            /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */ },
      "script"?:
        { "code": $micheline.michelson_v1.expression,
          "storage": $micheline.michelson_v1.expression },
      "counter": integer ∈ [-2^31-2, 2^31+2] }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }</pre>
    </div>
  

.. _GET_..--block_id--context--contracts--contract_id--balance :

**GET ../<block_id>/context/contracts/<contract_id>/balance**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--balancedescr', 'GET_..--block_id--context--contracts--contract_id--balance')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--balanceoutput', 'GET_..--block_id--context--contracts--contract_id--balance')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_id--balancedescr" class="GET_..--block_id--context--contracts--contract_id--balance tabcontent">
            <p>
            Access the balance of a contract.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_id--balanceoutput" class="GET_..--block_id--context--contracts--contract_id--balance tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--contracts--contract_id--counter :

**GET ../<block_id>/context/contracts/<contract_id>/counter**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--counterdescr', 'GET_..--block_id--context--contracts--contract_id--counter')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--counteroutput', 'GET_..--block_id--context--contracts--contract_id--counter')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_id--counterdescr" class="GET_..--block_id--context--contracts--contract_id--counter tabcontent">
            <p>
            Access the counter of a contract, if any.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_id--counteroutput" class="GET_..--block_id--context--contracts--contract_id--counter tabcontent">
    <pre>
    integer ∈ [-2^31-2, 2^31+2]</pre>
    </div>
  


.. _GET_..--block_id--context--contracts--contract_id--delegatable :

**GET ../<block_id>/context/contracts/<contract_id>/delegatable**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--delegatabledescr', 'GET_..--block_id--context--contracts--contract_id--delegatable')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--delegatableoutput', 'GET_..--block_id--context--contracts--contract_id--delegatable')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_id--delegatabledescr" class="GET_..--block_id--context--contracts--contract_id--delegatable tabcontent">
            <p>
            Tells if the contract delegate can be changed.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_id--delegatableoutput" class="GET_..--block_id--context--contracts--contract_id--delegatable tabcontent">
    <pre>
    boolean</pre>
    </div>
  


.. _GET_..--block_id--context--contracts--contract_id--delegate :

**GET ../<block_id>/context/contracts/<contract_id>/delegate**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--delegatedescr', 'GET_..--block_id--context--contracts--contract_id--delegate')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--delegateoutput', 'GET_..--block_id--context--contracts--contract_id--delegate')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_id--delegatedescr" class="GET_..--block_id--context--contracts--contract_id--delegate tabcontent">
            <p>
            Access the delegate of a contract, if any.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_id--delegateoutput" class="GET_..--block_id--context--contracts--contract_id--delegate tabcontent">
    <pre>
    string
    /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_..--block_id--context--contracts--contract_id--manager :

**GET ../<block_id>/context/contracts/<contract_id>/manager**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--managerdescr', 'GET_..--block_id--context--contracts--contract_id--manager')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--manageroutput', 'GET_..--block_id--context--contracts--contract_id--manager')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_id--managerdescr" class="GET_..--block_id--context--contracts--contract_id--manager tabcontent">
            <p>
            Access the manager of a contract.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_id--manageroutput" class="GET_..--block_id--context--contracts--contract_id--manager tabcontent">
    <pre>
    string
    /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_..--block_id--context--contracts--contract_id--manager_key :

**GET ../<block_id>/context/contracts/<contract_id>/manager_key**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--manager_keydescr', 'GET_..--block_id--context--contracts--contract_id--manager_key')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--manager_keyoutput', 'GET_..--block_id--context--contracts--contract_id--manager_key')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_id--manager_keydescr" class="GET_..--block_id--context--contracts--contract_id--manager_key tabcontent">
            <p>
            Access the manager of a contract.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_id--manager_keyoutput" class="GET_..--block_id--context--contracts--contract_id--manager_key tabcontent">
    <pre>
    { "manager":
        string
        /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
      "key"?:
        string
        /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */ }</pre>
    </div>
  


.. _GET_..--block_id--context--contracts--contract_id--script :

**GET ../<block_id>/context/contracts/<contract_id>/script**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--scriptdescr', 'GET_..--block_id--context--contracts--contract_id--script')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--scriptoutput', 'GET_..--block_id--context--contracts--contract_id--script')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_id--scriptdescr" class="GET_..--block_id--context--contracts--contract_id--script tabcontent">
            <p>
            Access the code and data of the contract.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_id--scriptoutput" class="GET_..--block_id--context--contracts--contract_id--script tabcontent">
    <pre>
    { "code": $micheline.michelson_v1.expression,
      "storage": $micheline.michelson_v1.expression }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }</pre>
    </div>
  


.. _GET_..--block_id--context--contracts--contract_id--spendable :

**GET ../<block_id>/context/contracts/<contract_id>/spendable**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--spendabledescr', 'GET_..--block_id--context--contracts--contract_id--spendable')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--spendableoutput', 'GET_..--block_id--context--contracts--contract_id--spendable')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_id--spendabledescr" class="GET_..--block_id--context--contracts--contract_id--spendable tabcontent">
            <p>
            Tells if the contract tokens can be spent by the manager.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_id--spendableoutput" class="GET_..--block_id--context--contracts--contract_id--spendable tabcontent">
    <pre>
    boolean</pre>
    </div>
  


.. _GET_..--block_id--context--contracts--contract_id--storage :

**GET ../<block_id>/context/contracts/<contract_id>/storage**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--storagedescr', 'GET_..--block_id--context--contracts--contract_id--storage')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--contracts--contract_id--storageoutput', 'GET_..--block_id--context--contracts--contract_id--storage')">Output format</button>
    </div><div id="GET_..--block_id--context--contracts--contract_id--storagedescr" class="GET_..--block_id--context--contracts--contract_id--storage tabcontent">
            <p>
            Access the data of the contract.</p>
            </div>
  <div id="GET_..--block_id--context--contracts--contract_id--storageoutput" class="GET_..--block_id--context--contracts--contract_id--storage tabcontent">
    <pre>
    $micheline.michelson_v1.expression
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }</pre>
    </div>
  


.. _GET_..--block_id--context--delegates :

**GET ../<block_id>/context/delegates?[active]&[inactive]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegatesdescr', 'GET_..--block_id--context--delegates')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegatesoutput', 'GET_..--block_id--context--delegates')">Output format</button>
    </div><div id="GET_..--block_id--context--delegatesdescr" class="GET_..--block_id--context--delegates tabcontent">
            <p>
            List all registred delegates.</p> <p>Optional query arguments :<ul><li><span class="query">active</span></li><li><span class="query">inactive</span></li></ul></p>
            </div>
  <div id="GET_..--block_id--context--delegatesoutput" class="GET_..--block_id--context--delegates tabcontent">
    <pre>
    [ string
    /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */ ... ]</pre>
    </div>
  

.. _GET_..--block_id--context--delegates--pkh :

**GET ../<block_id>/context/delegates/<pkh>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkhdescr', 'GET_..--block_id--context--delegates--pkh')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkhoutput', 'GET_..--block_id--context--delegates--pkh')">Output format</button>
    </div><div id="GET_..--block_id--context--delegates--pkhdescr" class="GET_..--block_id--context--delegates--pkh tabcontent">
            <p>
            Everything about a delegate.</p>
            </div>
  <div id="GET_..--block_id--context--delegates--pkhoutput" class="GET_..--block_id--context--delegates--pkh tabcontent">
    <pre>
    { "balance": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
      "frozen_balance":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "frozen_balance_by_cycle":
        [ { "cycle": integer ∈ [-2^31-2, 2^31+2],
            "deposit":
              /* Amount in mutez */
              integer ∈ [-2^31-2, 2^31+2] || string,
            "fees":
              /* Amount in mutez */
              integer ∈ [-2^31-2, 2^31+2] || string,
            "rewards":
              /* Amount in mutez */
              integer ∈ [-2^31-2, 2^31+2] || string } ... ],
      "staking_balance":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "delegated_contracts":
        [ string
        /* A contract ID (Base58Check-encoded) */ ... ],
      "delegated_balance":
        /* Amount in mutez */
        integer ∈ [-2^31-2, 2^31+2] || string,
      "deactivated": boolean,
      "grace_period": integer ∈ [-2^31-2, 2^31+2] }</pre>
    </div>
  

.. _GET_..--block_id--context--delegates--pkh--balance :

**GET ../<block_id>/context/delegates/<pkh>/balance**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--balancedescr', 'GET_..--block_id--context--delegates--pkh--balance')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--balanceoutput', 'GET_..--block_id--context--delegates--pkh--balance')">Output format</button>
    </div><div id="GET_..--block_id--context--delegates--pkh--balancedescr" class="GET_..--block_id--context--delegates--pkh--balance tabcontent">
            <p>
            Returns the full balance of a given delegate, including the frozen balances.</p>
            </div>
  <div id="GET_..--block_id--context--delegates--pkh--balanceoutput" class="GET_..--block_id--context--delegates--pkh--balance tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--delegates--pkh--deactivated :

**GET ../<block_id>/context/delegates/<pkh>/deactivated**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--deactivateddescr', 'GET_..--block_id--context--delegates--pkh--deactivated')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--deactivatedoutput', 'GET_..--block_id--context--delegates--pkh--deactivated')">Output format</button>
    </div><div id="GET_..--block_id--context--delegates--pkh--deactivateddescr" class="GET_..--block_id--context--delegates--pkh--deactivated tabcontent">
            <p>
            Returns whether the delegate is currently tagged as deactivated or not.</p>
            </div>
  <div id="GET_..--block_id--context--delegates--pkh--deactivatedoutput" class="GET_..--block_id--context--delegates--pkh--deactivated tabcontent">
    <pre>
    boolean</pre>
    </div>
  


.. _GET_..--block_id--context--delegates--pkh--delegated_balance :

**GET ../<block_id>/context/delegates/<pkh>/delegated_balance**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--delegated_balancedescr', 'GET_..--block_id--context--delegates--pkh--delegated_balance')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--delegated_balanceoutput', 'GET_..--block_id--context--delegates--pkh--delegated_balance')">Output format</button>
    </div><div id="GET_..--block_id--context--delegates--pkh--delegated_balancedescr" class="GET_..--block_id--context--delegates--pkh--delegated_balance tabcontent">
            <p>
            The includes the balance of all the contracts that delegates to it. This excludes the delegate own balance and its frozen balances.</p>
            </div>
  <div id="GET_..--block_id--context--delegates--pkh--delegated_balanceoutput" class="GET_..--block_id--context--delegates--pkh--delegated_balance tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--delegates--pkh--delegated_contracts :

**GET ../<block_id>/context/delegates/<pkh>/delegated_contracts**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--delegated_contractsdescr', 'GET_..--block_id--context--delegates--pkh--delegated_contracts')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--delegated_contractsoutput', 'GET_..--block_id--context--delegates--pkh--delegated_contracts')">Output format</button>
    </div><div id="GET_..--block_id--context--delegates--pkh--delegated_contractsdescr" class="GET_..--block_id--context--delegates--pkh--delegated_contracts tabcontent">
            <p>
            Returns the list of contract that delegates to a given delegate.</p>
            </div>
  <div id="GET_..--block_id--context--delegates--pkh--delegated_contractsoutput" class="GET_..--block_id--context--delegates--pkh--delegated_contracts tabcontent">
    <pre>
    [ string
    /* A contract ID (Base58Check-encoded) */ ... ]</pre>
    </div>
  


.. _GET_..--block_id--context--delegates--pkh--frozen_balance :

**GET ../<block_id>/context/delegates/<pkh>/frozen_balance**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--frozen_balancedescr', 'GET_..--block_id--context--delegates--pkh--frozen_balance')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--frozen_balanceoutput', 'GET_..--block_id--context--delegates--pkh--frozen_balance')">Output format</button>
    </div><div id="GET_..--block_id--context--delegates--pkh--frozen_balancedescr" class="GET_..--block_id--context--delegates--pkh--frozen_balance tabcontent">
            <p>
            Returns the total frozen balances of a given delegate, this includes the frozen deposits, rewards and fees.</p>
            </div>
  <div id="GET_..--block_id--context--delegates--pkh--frozen_balanceoutput" class="GET_..--block_id--context--delegates--pkh--frozen_balance tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycle :

**GET ../<block_id>/context/delegates/<pkh>/frozen_balance_by_cycle**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycledescr', 'GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycle')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycleoutput', 'GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycle')">Output format</button>
    </div><div id="GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycledescr" class="GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycle tabcontent">
            <p>
            Returns the frozen balances of a given delegate, indexed by the cycle by which it will be unfrozen</p>
            </div>
  <div id="GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycleoutput" class="GET_..--block_id--context--delegates--pkh--frozen_balance_by_cycle tabcontent">
    <pre>
    [ { "cycle": integer ∈ [-2^31-2, 2^31+2],
        "deposit": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
        "fees": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
        "rewards": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string } ... ]</pre>
    </div>
  


.. _GET_..--block_id--context--delegates--pkh--grace_period :

**GET ../<block_id>/context/delegates/<pkh>/grace_period**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--grace_perioddescr', 'GET_..--block_id--context--delegates--pkh--grace_period')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--grace_periodoutput', 'GET_..--block_id--context--delegates--pkh--grace_period')">Output format</button>
    </div><div id="GET_..--block_id--context--delegates--pkh--grace_perioddescr" class="GET_..--block_id--context--delegates--pkh--grace_period tabcontent">
            <p>
            Returns the cycle by the end of which the delegate might be deactivated, whether should she failed to execute any delegate action until then. A deactivated delegate might be reactivated (without loosing any rolls) by simply re-register as a delegate. For deactivated delegate this value contains the cycle by which they were deactivated.</p>
            </div>
  <div id="GET_..--block_id--context--delegates--pkh--grace_periodoutput" class="GET_..--block_id--context--delegates--pkh--grace_period tabcontent">
    <pre>
    integer ∈ [-2^31-2, 2^31+2]</pre>
    </div>
  


.. _GET_..--block_id--context--delegates--pkh--staking_balance :

**GET ../<block_id>/context/delegates/<pkh>/staking_balance**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--staking_balancedescr', 'GET_..--block_id--context--delegates--pkh--staking_balance')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--delegates--pkh--staking_balanceoutput', 'GET_..--block_id--context--delegates--pkh--staking_balance')">Output format</button>
    </div><div id="GET_..--block_id--context--delegates--pkh--staking_balancedescr" class="GET_..--block_id--context--delegates--pkh--staking_balance tabcontent">
            <p>
            Returns the total amount of token delegated to a given delegate. This includes the balance of all the contracts that delegates to it, but also the balance of the delegate itself and its frozen fees and deposits. The rewards do not count in the delegated balance until they are unfrozen.</p>
            </div>
  <div id="GET_..--block_id--context--delegates--pkh--staking_balanceoutput" class="GET_..--block_id--context--delegates--pkh--staking_balance tabcontent">
    <pre>
    /* Amount in mutez */
    integer ∈ [-2^31-2, 2^31+2] || string</pre>
    </div>
  


.. _GET_..--block_id--context--nonces--block_level :

**GET ../<block_id>/context/nonces/<block_level>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--nonces--block_leveldescr', 'GET_..--block_id--context--nonces--block_level')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--nonces--block_leveloutput', 'GET_..--block_id--context--nonces--block_level')">Output format</button>
    </div><div id="GET_..--block_id--context--nonces--block_leveldescr" class="GET_..--block_id--context--nonces--block_level tabcontent">
            <p>
            Info about the nonce of a previous block.</p>
            </div>
  <div id="GET_..--block_id--context--nonces--block_leveloutput" class="GET_..--block_id--context--nonces--block_level tabcontent">
    <pre>
    { "nonce": /^[a-zA-Z0-9]+$/ }
    || { "hash": string /* A nonce hash (Base58Check-encoded) */ }
    || {  }</pre>
    </div>
  


.. _GET_..--block_id--context--raw--bytes :

**GET ../<block_id>/context/raw/bytes?[depth=<int>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--context--raw--bytesdescr', 'GET_..--block_id--context--raw--bytes')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--context--raw--bytesoutput', 'GET_..--block_id--context--raw--bytes')">Output format</button>
    </div><div id="GET_..--block_id--context--raw--bytesdescr" class="GET_..--block_id--context--raw--bytes tabcontent">
            <p>
            Returns the raw context.</p> <p>Optional query arguments :<ul><li><span class="query">depth = &lt;int&gt;</span></li></ul></p>
            </div>
  <div id="GET_..--block_id--context--raw--bytesoutput" class="GET_..--block_id--context--raw--bytes tabcontent">
    <pre>
    $raw_context
    $raw_context: /^[a-zA-Z0-9]+$/ || { *: $raw_context } || null</pre>
    </div>
  


.. _GET_..--block_id--hash :

**GET ../<block_id>/hash**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--hashdescr', 'GET_..--block_id--hash')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--hashoutput', 'GET_..--block_id--hash')">Output format</button>
    </div><div id="GET_..--block_id--hashdescr" class="GET_..--block_id--hash tabcontent">
            <p>
            The block's hash, its unique identifier.</p>
            </div>
  <div id="GET_..--block_id--hashoutput" class="GET_..--block_id--hash tabcontent">
    <pre>
    string
    /* A block identifier (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_..--block_id--header :

**GET ../<block_id>/header**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--headerdescr', 'GET_..--block_id--header')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--headeroutput', 'GET_..--block_id--header')">Output format</button>
    </div><div id="GET_..--block_id--headerdescr" class="GET_..--block_id--header tabcontent">
            <p>
            The whole block header.</p>
            </div>
  <div id="GET_..--block_id--headeroutput" class="GET_..--block_id--header tabcontent">
    <pre>
    $block_header
    $block_header:
      { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "chain_id": string /* Network identifier (Base58Check-encoded) */,
        "hash": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_..--block_id--header--protocol_data :

**GET ../<block_id>/header/protocol_data**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--protocol_datadescr', 'GET_..--block_id--header--protocol_data')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--protocol_dataoutput', 'GET_..--block_id--header--protocol_data')">Output format</button>
    </div><div id="GET_..--block_id--header--protocol_datadescr" class="GET_..--block_id--header--protocol_data tabcontent">
            <p>
            The version-specific fragment of the block header.</p>
            </div>
  <div id="GET_..--block_id--header--protocol_dataoutput" class="GET_..--block_id--header--protocol_data tabcontent">
    <pre>
    { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
      "priority": integer ∈ [0, 2^16-1],
      "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
      "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
      "signature":
        string
        /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }</pre>
    </div>
  


.. _GET_..--block_id--header--shell :

**GET ../<block_id>/header/shell**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--shelldescr', 'GET_..--block_id--header--shell')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--shelloutput', 'GET_..--block_id--header--shell')">Output format</button>
    </div><div id="GET_..--block_id--header--shelldescr" class="GET_..--block_id--header--shell tabcontent">
            <p>
            The shell-specific fragment of the block header.</p>
            </div>
  <div id="GET_..--block_id--header--shelloutput" class="GET_..--block_id--header--shell tabcontent">
    <pre>
    $block_header.shell
    $block_header.shell:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */ }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_..--block_id--header--shell--context_hash :

**GET ../<block_id>/header/shell/context_hash**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--shell--context_hashdescr', 'GET_..--block_id--header--shell--context_hash')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--shell--context_hashoutput', 'GET_..--block_id--header--shell--context_hash')">Output format</button>
    </div><div id="GET_..--block_id--header--shell--context_hashdescr" class="GET_..--block_id--header--shell--context_hash tabcontent">
            <p>
            The hash of the resulting validation context.</p>
            </div>
  <div id="GET_..--block_id--header--shell--context_hashoutput" class="GET_..--block_id--header--shell--context_hash tabcontent">
    <pre>
    string
    /* A hash of context (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_..--block_id--header--shell--fitness :

**GET ../<block_id>/header/shell/fitness**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--shell--fitnessdescr', 'GET_..--block_id--header--shell--fitness')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--shell--fitnessoutput', 'GET_..--block_id--header--shell--fitness')">Output format</button>
    </div><div id="GET_..--block_id--header--shell--fitnessdescr" class="GET_..--block_id--header--shell--fitness tabcontent">
            <p>
            The block's fitness.</p>
            </div>
  <div id="GET_..--block_id--header--shell--fitnessoutput" class="GET_..--block_id--header--shell--fitness tabcontent">
    <pre>
    [ /^[a-zA-Z0-9]+$/ ... ]
    /* Tezos block fitness */</pre>
    </div>
  


.. _GET_..--block_id--header--shell--level :

**GET ../<block_id>/header/shell/level**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--shell--leveldescr', 'GET_..--block_id--header--shell--level')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--shell--leveloutput', 'GET_..--block_id--header--shell--level')">Output format</button>
    </div><div id="GET_..--block_id--header--shell--leveldescr" class="GET_..--block_id--header--shell--level tabcontent">
            <p>
            The block's level.</p>
            </div>
  <div id="GET_..--block_id--header--shell--leveloutput" class="GET_..--block_id--header--shell--level tabcontent">
    <pre>
    integer ∈ [-2^31-2, 2^31+2]</pre>
    </div>
  


.. _GET_..--block_id--header--shell--operations_hash :

**GET ../<block_id>/header/shell/operations_hash**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--shell--operations_hashdescr', 'GET_..--block_id--header--shell--operations_hash')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--shell--operations_hashoutput', 'GET_..--block_id--header--shell--operations_hash')">Output format</button>
    </div><div id="GET_..--block_id--header--shell--operations_hashdescr" class="GET_..--block_id--header--shell--operations_hash tabcontent">
            <p>
            The hash of merkle tree of the operations included in the block.</p>
            </div>
  <div id="GET_..--block_id--header--shell--operations_hashoutput" class="GET_..--block_id--header--shell--operations_hash tabcontent">
    <pre>
    string
    /* A list of list of operations (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_..--block_id--header--shell--predecessor :

**GET ../<block_id>/header/shell/predecessor**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--shell--predecessordescr', 'GET_..--block_id--header--shell--predecessor')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--shell--predecessoroutput', 'GET_..--block_id--header--shell--predecessor')">Output format</button>
    </div><div id="GET_..--block_id--header--shell--predecessordescr" class="GET_..--block_id--header--shell--predecessor tabcontent">
            <p>
            The previous block's id.</p>
            </div>
  <div id="GET_..--block_id--header--shell--predecessoroutput" class="GET_..--block_id--header--shell--predecessor tabcontent">
    <pre>
    string
    /* A block identifier (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_..--block_id--header--shell--proto_level :

**GET ../<block_id>/header/shell/proto_level**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--shell--proto_leveldescr', 'GET_..--block_id--header--shell--proto_level')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--shell--proto_leveloutput', 'GET_..--block_id--header--shell--proto_level')">Output format</button>
    </div><div id="GET_..--block_id--header--shell--proto_leveldescr" class="GET_..--block_id--header--shell--proto_level tabcontent">
            <p>
            The block's protocol level (modulo 256).</p>
            </div>
  <div id="GET_..--block_id--header--shell--proto_leveloutput" class="GET_..--block_id--header--shell--proto_level tabcontent">
    <pre>
    integer ∈ [0, 255]</pre>
    </div>
  


.. _GET_..--block_id--header--shell--timestamp :

**GET ../<block_id>/header/shell/timestamp**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--shell--timestampdescr', 'GET_..--block_id--header--shell--timestamp')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--shell--timestampoutput', 'GET_..--block_id--header--shell--timestamp')">Output format</button>
    </div><div id="GET_..--block_id--header--shell--timestampdescr" class="GET_..--block_id--header--shell--timestamp tabcontent">
            <p>
            The block's timestamp.</p>
            </div>
  <div id="GET_..--block_id--header--shell--timestampoutput" class="GET_..--block_id--header--shell--timestamp tabcontent">
    <pre>
    /* timestamp */
    $timestamp || integer ∈ [-2^31-2, 2^31+2] || string
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _GET_..--block_id--header--shell--validation_passes :

**GET ../<block_id>/header/shell/validation_passes**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--header--shell--validation_passesdescr', 'GET_..--block_id--header--shell--validation_passes')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--header--shell--validation_passesoutput', 'GET_..--block_id--header--shell--validation_passes')">Output format</button>
    </div><div id="GET_..--block_id--header--shell--validation_passesdescr" class="GET_..--block_id--header--shell--validation_passes tabcontent">
            <p>
            The number of validation passes for the block.</p>
            </div>
  <div id="GET_..--block_id--header--shell--validation_passesoutput" class="GET_..--block_id--header--shell--validation_passes tabcontent">
    <pre>
    integer ∈ [0, 255]</pre>
    </div>
  


.. _GET_..--block_id--helpers--baking_rights :

**GET ../<block_id>/helpers/baking_rights?(level=<block_level>)\*&(cycle=<block_cycle>)\*&(delegate=<pkh>)\*&[max_priority=<int>]&[all]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--helpers--baking_rightsdescr', 'GET_..--block_id--helpers--baking_rights')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--helpers--baking_rightsoutput', 'GET_..--block_id--helpers--baking_rights')">Output format</button>
    </div><div id="GET_..--block_id--helpers--baking_rightsdescr" class="GET_..--block_id--helpers--baking_rights tabcontent">
            <p>
            ...FIXME...</p> <p>Optional query arguments :<ul><li><span class="query">level = &lt;block_level&gt;</span></li><li><span class="query">cycle = &lt;block_cycle&gt;</span></li><li><span class="query">delegate = &lt;pkh&gt;</span></li><li><span class="query">max_priority = &lt;int&gt;</span></li><li><span class="query">all</span></li></ul></p>
            </div>
  <div id="GET_..--block_id--helpers--baking_rightsoutput" class="GET_..--block_id--helpers--baking_rights tabcontent">
    <pre>
    [ { "level": integer ∈ [-2^31-2, 2^31+2],
        "delegate":
          string
          /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "timestamp"?:
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _GET_..--block_id--helpers--complete--prefix :

**GET ../<block_id>/helpers/complete/<prefix>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--helpers--complete--prefixdescr', 'GET_..--block_id--helpers--complete--prefix')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--helpers--complete--prefixoutput', 'GET_..--block_id--helpers--complete--prefix')">Output format</button>
    </div><div id="GET_..--block_id--helpers--complete--prefixdescr" class="GET_..--block_id--helpers--complete--prefix tabcontent">
            <p>
            Try to complete a prefix of a Base58Check-encoded data. This RPC is actually able to complete hashes of block, operations, public_keys and contracts.</p>
            </div>
  <div id="GET_..--block_id--helpers--complete--prefixoutput" class="GET_..--block_id--helpers--complete--prefix tabcontent">
    <pre>
    [ string ... ]</pre>
    </div>
  


.. _GET_..--block_id--helpers--current_level :

**GET ../<block_id>/helpers/current_level?[offset=<int32>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--helpers--current_leveldescr', 'GET_..--block_id--helpers--current_level')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--helpers--current_leveloutput', 'GET_..--block_id--helpers--current_level')">Output format</button>
    </div><div id="GET_..--block_id--helpers--current_leveldescr" class="GET_..--block_id--helpers--current_level tabcontent">
            <p>
            ...</p> <p>Optional query arguments :<ul><li><span class="query">offset = &lt;int32&gt;</span></li></ul></p>
            </div>
  <div id="GET_..--block_id--helpers--current_leveloutput" class="GET_..--block_id--helpers--current_level tabcontent">
    <pre>
    { "level": integer ∈ [-2^31-2, 2^31+2],
      "level_position": integer ∈ [-2^31-2, 2^31+2],
      "cycle": integer ∈ [-2^31-2, 2^31+2],
      "cycle_position": integer ∈ [-2^31-2, 2^31+2],
      "voting_period": integer ∈ [-2^31-2, 2^31+2],
      "voting_period_position": integer ∈ [-2^31-2, 2^31+2],
      "expected_commitment": boolean }</pre>
    </div>
  


.. _GET_..--block_id--helpers--endorsing_rights :

**GET ../<block_id>/helpers/endorsing_rights?(level=<block_level>)\*&(cycle=<block_cycle>)\*&(delegate=<pkh>)\***

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--helpers--endorsing_rightsdescr', 'GET_..--block_id--helpers--endorsing_rights')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--helpers--endorsing_rightsoutput', 'GET_..--block_id--helpers--endorsing_rights')">Output format</button>
    </div><div id="GET_..--block_id--helpers--endorsing_rightsdescr" class="GET_..--block_id--helpers--endorsing_rights tabcontent">
            <p>
            ...FIXME...</p> <p>Optional query arguments :<ul><li><span class="query">level = &lt;block_level&gt;</span></li><li><span class="query">cycle = &lt;block_cycle&gt;</span></li><li><span class="query">delegate = &lt;pkh&gt;</span></li></ul></p>
            </div>
  <div id="GET_..--block_id--helpers--endorsing_rightsoutput" class="GET_..--block_id--helpers--endorsing_rights tabcontent">
    <pre>
    [ { "level": integer ∈ [-2^31-2, 2^31+2],
        "delegate":
          string
          /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
        "slots": [ integer ∈ [0, 2^16-1] ... ],
        "estimated_time"?:
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _POST_..--block_id--helpers--forge--operations :

**POST ../<block_id>/helpers/forge/operations**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--forge--operationsdescr', 'POST_..--block_id--helpers--forge--operations')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--forge--operationsinput', 'POST_..--block_id--helpers--forge--operations')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--forge--operationsoutput', 'POST_..--block_id--helpers--forge--operations')">Output format</button>
    </div><div id="POST_..--block_id--helpers--forge--operationsdescr" class="POST_..--block_id--helpers--forge--operations tabcontent">
            <p>
            Forge an operation</p>
            </div>
  <div id="POST_..--block_id--helpers--forge--operationsinput" class="POST_..--block_id--helpers--forge--operations tabcontent">
    <pre>
    $operation.alpha.unsigned_operation
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_header.alpha.full_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $operation.alpha.contents:
      { "kind": "endorsement",
        "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] }
      || { "kind": "seed_nonce_revelation",
           "level": integer ∈ [-2^31-2, 2^31+2],
           "nonce": /^[a-zA-Z0-9]+$/ }
      || { "kind": "double_endorsement_evidence",
           "op1":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "op2":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ } }
      || { "kind": "double_baking_evidence",
           "bh1": $block_header.alpha.full_header,
           "bh2": $block_header.alpha.full_header }
      || { "kind": "activate_account",
           "pkh": string /* An Ed25519 public key hash (Base58Check-encoded) */,
           "secret": /^[a-zA-Z0-9]+$/ }
      || { "kind": "proposals",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposals":
             [ string
             /* A Tezos protocol ID (Base58Check-encoded) */ ... ] }
      || { "kind": "ballot",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposal": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "ballot": "nay" | "yay" | "pass" }
      || { "kind": "reveal",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "public_key":
             string
             /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */ }
      || { "kind": "transaction",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression }
      || { "kind": "origination",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression } }
      || { "kind": "delegation",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */ }
      || { "kind": "activate_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */ }
      || { "kind": "activate_test_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */ }
    $operation.alpha.unsigned_operation:
      /* Unsigned Alpha operation */
      { "branch": string /* A block identifier (Base58Check-encoded) */,
        "contents": [ $operation.alpha.contents ... ] }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  <div id="POST_..--block_id--helpers--forge--operationsoutput" class="POST_..--block_id--helpers--forge--operations tabcontent">
    <pre>
    /^[a-zA-Z0-9]+$/</pre>
    </div>
  


.. _POST_..--block_id--helpers--forge--protocol_data :

**POST ../<block_id>/helpers/forge/protocol_data**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--forge--protocol_datadescr', 'POST_..--block_id--helpers--forge--protocol_data')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--forge--protocol_datainput', 'POST_..--block_id--helpers--forge--protocol_data')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--forge--protocol_dataoutput', 'POST_..--block_id--helpers--forge--protocol_data')">Output format</button>
    </div><div id="POST_..--block_id--helpers--forge--protocol_datadescr" class="POST_..--block_id--helpers--forge--protocol_data tabcontent">
            <p>
            Forge the protocol-specific part of a block header</p>
            </div>
  <div id="POST_..--block_id--helpers--forge--protocol_datainput" class="POST_..--block_id--helpers--forge--protocol_data tabcontent">
    <pre>
    { "priority": integer ∈ [0, 2^16-1],
      "nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
      "proof_of_work_nonce"?: /^[a-zA-Z0-9]+$/ }</pre>
    </div>
  <div id="POST_..--block_id--helpers--forge--protocol_dataoutput" class="POST_..--block_id--helpers--forge--protocol_data tabcontent">
    <pre>
    { "protocol_data": /^[a-zA-Z0-9]+$/ }</pre>
    </div>
  


.. _POST_..--block_id--helpers--forge_block_header :

**POST ../<block_id>/helpers/forge_block_header**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--forge_block_headerdescr', 'POST_..--block_id--helpers--forge_block_header')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--forge_block_headerinput', 'POST_..--block_id--helpers--forge_block_header')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--forge_block_headeroutput', 'POST_..--block_id--helpers--forge_block_header')">Output format</button>
    </div><div id="POST_..--block_id--helpers--forge_block_headerdescr" class="POST_..--block_id--helpers--forge_block_header tabcontent">
            <p>
            Forge a block header</p>
            </div>
  <div id="POST_..--block_id--helpers--forge_block_headerinput" class="POST_..--block_id--helpers--forge_block_header tabcontent">
    <pre>
    { "level": integer ∈ [-2^31-2, 2^31+2],
      "proto": integer ∈ [0, 255],
      "predecessor": string /* A block identifier (Base58Check-encoded) */,
      "timestamp":
        /* timestamp */
        $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
      "validation_pass": integer ∈ [0, 255],
      "operations_hash":
        string
        /* A list of list of operations (Base58Check-encoded) */,
      "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
      "context": string /* A hash of context (Base58Check-encoded) */,
      "protocol_data": /^[a-zA-Z0-9]+$/ }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  <div id="POST_..--block_id--helpers--forge_block_headeroutput" class="POST_..--block_id--helpers--forge_block_header tabcontent">
    <pre>
    { "block": /^[a-zA-Z0-9]+$/ }</pre>
    </div>
  


.. _GET_..--block_id--helpers--levels_in_current_cycle :

**GET ../<block_id>/helpers/levels_in_current_cycle?[offset=<int32>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--helpers--levels_in_current_cycledescr', 'GET_..--block_id--helpers--levels_in_current_cycle')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--helpers--levels_in_current_cycleoutput', 'GET_..--block_id--helpers--levels_in_current_cycle')">Output format</button>
    </div><div id="GET_..--block_id--helpers--levels_in_current_cycledescr" class="GET_..--block_id--helpers--levels_in_current_cycle tabcontent">
            <p>
            Levels of a cycle</p> <p>Optional query arguments :<ul><li><span class="query">offset = &lt;int32&gt;</span></li></ul></p>
            </div>
  <div id="GET_..--block_id--helpers--levels_in_current_cycleoutput" class="GET_..--block_id--helpers--levels_in_current_cycle tabcontent">
    <pre>
    { /* levels of a cycle */
      "first": integer ∈ [-2^31-2, 2^31+2],
      "last": integer ∈ [-2^31-2, 2^31+2] }</pre>
    </div>
  


.. _POST_..--block_id--helpers--parse--block :

**POST ../<block_id>/helpers/parse/block**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--parse--blockdescr', 'POST_..--block_id--helpers--parse--block')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--parse--blockinput', 'POST_..--block_id--helpers--parse--block')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--parse--blockoutput', 'POST_..--block_id--helpers--parse--block')">Output format</button>
    </div><div id="POST_..--block_id--helpers--parse--blockdescr" class="POST_..--block_id--helpers--parse--block tabcontent">
            <p>
            Parse a block</p>
            </div>
  <div id="POST_..--block_id--helpers--parse--blockinput" class="POST_..--block_id--helpers--parse--block tabcontent">
    <pre>
    { "level": integer ∈ [-2^31-2, 2^31+2],
      "proto": integer ∈ [0, 255],
      "predecessor": string /* A block identifier (Base58Check-encoded) */,
      "timestamp":
        /* timestamp */
        $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
      "validation_pass": integer ∈ [0, 255],
      "operations_hash":
        string
        /* A list of list of operations (Base58Check-encoded) */,
      "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
      "context": string /* A hash of context (Base58Check-encoded) */,
      "protocol_data": /^[a-zA-Z0-9]+$/ }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  <div id="POST_..--block_id--helpers--parse--blockoutput" class="POST_..--block_id--helpers--parse--block tabcontent">
    <pre>
    $block_header.alpha.signed_contents
    $block_header.alpha.signed_contents:
      { "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }</pre>
    </div>
  


.. _POST_..--block_id--helpers--parse--operations :

**POST ../<block_id>/helpers/parse/operations**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--parse--operationsdescr', 'POST_..--block_id--helpers--parse--operations')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--parse--operationsinput', 'POST_..--block_id--helpers--parse--operations')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--parse--operationsoutput', 'POST_..--block_id--helpers--parse--operations')">Output format</button>
    </div><div id="POST_..--block_id--helpers--parse--operationsdescr" class="POST_..--block_id--helpers--parse--operations tabcontent">
            <p>
            Parse operations</p>
            </div>
  <div id="POST_..--block_id--helpers--parse--operationsinput" class="POST_..--block_id--helpers--parse--operations tabcontent">
    <pre>
    { "operations":
        [ { "branch": string /* A block identifier (Base58Check-encoded) */,
            "data": /^[a-zA-Z0-9]+$/ } ... ],
      "check_signature"?: boolean }</pre>
    </div>
  <div id="POST_..--block_id--helpers--parse--operationsoutput" class="POST_..--block_id--helpers--parse--operations tabcontent">
    <pre>
    [ { "branch": string /* A block identifier (Base58Check-encoded) */,
        "contents": [ $operation.alpha.contents ... ],
        "signature"?:
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ } ... ]
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_header.alpha.full_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $operation.alpha.contents:
      { "kind": "endorsement",
        "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] }
      || { "kind": "seed_nonce_revelation",
           "level": integer ∈ [-2^31-2, 2^31+2],
           "nonce": /^[a-zA-Z0-9]+$/ }
      || { "kind": "double_endorsement_evidence",
           "op1":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "op2":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ } }
      || { "kind": "double_baking_evidence",
           "bh1": $block_header.alpha.full_header,
           "bh2": $block_header.alpha.full_header }
      || { "kind": "activate_account",
           "pkh": string /* An Ed25519 public key hash (Base58Check-encoded) */,
           "secret": /^[a-zA-Z0-9]+$/ }
      || { "kind": "proposals",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposals":
             [ string
             /* A Tezos protocol ID (Base58Check-encoded) */ ... ] }
      || { "kind": "ballot",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposal": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "ballot": "nay" | "yay" | "pass" }
      || { "kind": "reveal",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "public_key":
             string
             /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */ }
      || { "kind": "transaction",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression }
      || { "kind": "origination",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression } }
      || { "kind": "delegation",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */ }
      || { "kind": "activate_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */ }
      || { "kind": "activate_test_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */ }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _POST_..--block_id--helpers--preapply--block :

**POST ../<block_id>/helpers/preapply/block?[sort]&[timestamp=<date>]**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--blockdescr', 'POST_..--block_id--helpers--preapply--block')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--blockinput', 'POST_..--block_id--helpers--preapply--block')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--blockoutput', 'POST_..--block_id--helpers--preapply--block')">Output format</button>
    </div><div id="POST_..--block_id--helpers--preapply--blockdescr" class="POST_..--block_id--helpers--preapply--block tabcontent">
            <p>
            Simulate the validation of a block that would contain the given operations and return the resulting fitness and context hash.</p> <p>Optional query arguments :<ul><li><span class="query">sort</span></li><li><span class="query">timestamp = &lt;date&gt;</span></li></ul></p>
            </div>
  <div id="POST_..--block_id--helpers--preapply--blockinput" class="POST_..--block_id--helpers--preapply--block tabcontent">
    <pre>
    { "protocol_data":
        { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
          "priority": integer ∈ [0, 2^16-1],
          "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
          "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
          "signature":
            string
            /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
      "operations": [ [ $next_operation ... ] ... ] }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_header.alpha.full_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $next_operation:
      /* Operations param */
      { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "branch": string /* A block identifier (Base58Check-encoded) */,
        "contents": [ $operation.alpha.contents ... ],
        "signature"?:
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $operation.alpha.contents:
      { "kind": "endorsement",
        "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] }
      || { "kind": "seed_nonce_revelation",
           "level": integer ∈ [-2^31-2, 2^31+2],
           "nonce": /^[a-zA-Z0-9]+$/ }
      || { "kind": "double_endorsement_evidence",
           "op1":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "op2":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ } }
      || { "kind": "double_baking_evidence",
           "bh1": $block_header.alpha.full_header,
           "bh2": $block_header.alpha.full_header }
      || { "kind": "activate_account",
           "pkh": string /* An Ed25519 public key hash (Base58Check-encoded) */,
           "secret": /^[a-zA-Z0-9]+$/ }
      || { "kind": "proposals",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposals":
             [ string
             /* A Tezos protocol ID (Base58Check-encoded) */ ... ] }
      || { "kind": "ballot",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposal": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "ballot": "nay" | "yay" | "pass" }
      || { "kind": "reveal",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "public_key":
             string
             /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */ }
      || { "kind": "transaction",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression }
      || { "kind": "origination",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression } }
      || { "kind": "delegation",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */ }
      || { "kind": "activate_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */ }
      || { "kind": "activate_test_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */ }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  <div id="POST_..--block_id--helpers--preapply--blockoutput" class="POST_..--block_id--helpers--preapply--block tabcontent">
    <pre>
    { "shell_header": $block_header.shell,
      "operations":
        [ { "applied":
              [ { "hash":
                    string
                    /* A Tezos operation ID (Base58Check-encoded) */,
                  "branch":
                    string
                    /* A block identifier (Base58Check-encoded) */,
                  "data": /^[a-zA-Z0-9]+$/ } ... ],
            "refused":
              [ { "hash":
                    string
                    /* A Tezos operation ID (Base58Check-encoded) */,
                  "branch":
                    string
                    /* A block identifier (Base58Check-encoded) */,
                  "data": /^[a-zA-Z0-9]+$/,
                  "error":
                    any
                    /* The full list of error is available with the global RPC
                       `GET errors` */ } ... ],
            "branch_refused":
              [ { "hash":
                    string
                    /* A Tezos operation ID (Base58Check-encoded) */,
                  "branch":
                    string
                    /* A block identifier (Base58Check-encoded) */,
                  "data": /^[a-zA-Z0-9]+$/,
                  "error":
                    any
                    /* The full list of error is available with the global RPC
                       `GET errors` */ } ... ],
            "branch_delayed":
              [ { "hash":
                    string
                    /* A Tezos operation ID (Base58Check-encoded) */,
                  "branch":
                    string
                    /* A block identifier (Base58Check-encoded) */,
                  "data": /^[a-zA-Z0-9]+$/,
                  "error":
                    any
                    /* The full list of error is available with the global RPC
                       `GET errors` */ } ... ] } ... ] }
    $block_header.shell:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */ }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _POST_..--block_id--helpers--preapply--operations :

**POST ../<block_id>/helpers/preapply/operations**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--operationsdescr', 'POST_..--block_id--helpers--preapply--operations')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--operationsinput', 'POST_..--block_id--helpers--preapply--operations')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--preapply--operationsoutput', 'POST_..--block_id--helpers--preapply--operations')">Output format</button>
    </div><div id="POST_..--block_id--helpers--preapply--operationsdescr" class="POST_..--block_id--helpers--preapply--operations tabcontent">
            <p>
            Simulate the validation of an operation.</p>
            </div>
  <div id="POST_..--block_id--helpers--preapply--operationsinput" class="POST_..--block_id--helpers--preapply--operations tabcontent">
    <pre>
    [ $next_operation ... ]
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_header.alpha.full_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $next_operation:
      /* Operations param */
      { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "branch": string /* A block identifier (Base58Check-encoded) */,
        "contents": [ $operation.alpha.contents ... ],
        "signature"?:
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $operation.alpha.contents:
      { "kind": "endorsement",
        "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] }
      || { "kind": "seed_nonce_revelation",
           "level": integer ∈ [-2^31-2, 2^31+2],
           "nonce": /^[a-zA-Z0-9]+$/ }
      || { "kind": "double_endorsement_evidence",
           "op1":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "op2":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ } }
      || { "kind": "double_baking_evidence",
           "bh1": $block_header.alpha.full_header,
           "bh2": $block_header.alpha.full_header }
      || { "kind": "activate_account",
           "pkh": string /* An Ed25519 public key hash (Base58Check-encoded) */,
           "secret": /^[a-zA-Z0-9]+$/ }
      || { "kind": "proposals",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposals":
             [ string
             /* A Tezos protocol ID (Base58Check-encoded) */ ... ] }
      || { "kind": "ballot",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposal": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "ballot": "nay" | "yay" | "pass" }
      || { "kind": "reveal",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "public_key":
             string
             /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */ }
      || { "kind": "transaction",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression }
      || { "kind": "origination",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression } }
      || { "kind": "delegation",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */ }
      || { "kind": "activate_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */ }
      || { "kind": "activate_test_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */ }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  <div id="POST_..--block_id--helpers--preapply--operationsoutput" class="POST_..--block_id--helpers--preapply--operations tabcontent">
    <pre>
    [ $operation.alpha.operation_with_metadata ... ]
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_header.alpha.full_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $error:
      /* ... FIXME ... */
      any
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $operation.alpha.internal_operation_result:
      { "kind": "reveal",
        "source": $contract_id,
        "nonce": integer ∈ [0, 2^16-1],
        "public_key":
          string
          /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
        "result": $operation.alpha.operation_result.reveal }
      || { "kind": "transaction",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "result": $operation.alpha.operation_result.transaction }
      || { "kind": "origination",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "result": $operation.alpha.operation_result.origination }
      || { "kind": "delegation",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "result": $operation.alpha.operation_result.delegation }
    $operation.alpha.operation_contents_and_result:
      { "kind": "endorsement",
        "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ],
        "metadata":
          { "delegate":
              string
              /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
            "slots": [ integer ∈ [0, 255] ... ] } }
      || { "kind": "seed_nonce_revelation",
           "level": integer ∈ [-2^31-2, 2^31+2],
           "nonce": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_endorsement_evidence",
           "op1":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "op2":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_baking_evidence",
           "bh1": $block_header.alpha.full_header,
           "bh2": $block_header.alpha.full_header,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "activate_account",
           "pkh": string /* An Ed25519 public key hash (Base58Check-encoded) */,
           "secret": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "proposals",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposals":
             [ string
             /* A Tezos protocol ID (Base58Check-encoded) */ ... ],
           "metadata": {  } }
      || { "kind": "ballot",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposal": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "ballot": "nay" | "yay" | "pass",
           "metadata": {  } }
      || { "kind": "reveal",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "public_key":
             string
             /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.reveal,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "transaction",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.transaction,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "origination",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.origination,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "delegation",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.delegation,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "activate_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
      || { "kind": "activate_test_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
    $operation.alpha.operation_result.delegation:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.origination:
      { "status": "applied",
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.reveal:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.transaction:
      { "status": "applied",
        "storage"?: $micheline.michelson_v1.expression,
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_with_metadata:
      { "contents": [ $operation.alpha.operation_contents_and_result ... ],
        "signature"?:
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $operation_metadata.alpha.balance_updates:
      [ { "kind": "contract",
          "contract": $contract_id,
          "credited":
            /* Amount in mutez */
            integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "contract",
             "contract": $contract_id,
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _POST_..--block_id--helpers--scripts--hash_data :

**POST ../<block_id>/helpers/scripts/hash_data**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--hash_datadescr', 'POST_..--block_id--helpers--scripts--hash_data')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--hash_datainput', 'POST_..--block_id--helpers--scripts--hash_data')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--hash_dataoutput', 'POST_..--block_id--helpers--scripts--hash_data')">Output format</button>
    </div><div id="POST_..--block_id--helpers--scripts--hash_datadescr" class="POST_..--block_id--helpers--scripts--hash_data tabcontent">
            <p>
            Computes the hash of some data expression using the same algorithm as script instruction H</p>
            </div>
  <div id="POST_..--block_id--helpers--scripts--hash_datainput" class="POST_..--block_id--helpers--scripts--hash_data tabcontent">
    <pre>
    { "data": $micheline.michelson_v1.expression,
      "type": $micheline.michelson_v1.expression,
      "gas"?: $bignum }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }</pre>
    </div>
  <div id="POST_..--block_id--helpers--scripts--hash_dataoutput" class="POST_..--block_id--helpers--scripts--hash_data tabcontent">
    <pre>
    { "hash": string,
      "gas": $bignum || "unaccounted" }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string</pre>
    </div>
  


.. _POST_..--block_id--helpers--scripts--run_code :

**POST ../<block_id>/helpers/scripts/run_code**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--run_codedescr', 'POST_..--block_id--helpers--scripts--run_code')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--run_codeinput', 'POST_..--block_id--helpers--scripts--run_code')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--run_codeoutput', 'POST_..--block_id--helpers--scripts--run_code')">Output format</button>
    </div><div id="POST_..--block_id--helpers--scripts--run_codedescr" class="POST_..--block_id--helpers--scripts--run_code tabcontent">
            <p>
            Run a piece of code in the current context</p>
            </div>
  <div id="POST_..--block_id--helpers--scripts--run_codeinput" class="POST_..--block_id--helpers--scripts--run_code tabcontent">
    <pre>
    { "script": $micheline.michelson_v1.expression,
      "storage": $micheline.michelson_v1.expression,
      "input": $micheline.michelson_v1.expression,
      "amount": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
      "contract": $contract_id }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }</pre>
    </div>
  <div id="POST_..--block_id--helpers--scripts--run_codeoutput" class="POST_..--block_id--helpers--scripts--run_code tabcontent">
    <pre>
    { "storage": $micheline.michelson_v1.expression,
      "operations": [ $operation.alpha.internal_operation ... ],
      "big_map_diff"?:
        [ [ string, $micheline.michelson_v1.expression || null ] ... ] }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $operation.alpha.internal_operation:
      { "source": $contract_id,
        "nonce": integer ∈ [0, 2^16-1],
        "kind": "reveal",
        "public_key":
          string
          /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */ }
      || { "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "kind": "transaction",
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression }
      || { "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "kind": "origination",
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression } }
      || { "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "kind": "delegation",
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */ }</pre>
    </div>
  


.. _POST_..--block_id--helpers--scripts--trace_code :

**POST ../<block_id>/helpers/scripts/trace_code**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--trace_codedescr', 'POST_..--block_id--helpers--scripts--trace_code')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--trace_codeinput', 'POST_..--block_id--helpers--scripts--trace_code')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--trace_codeoutput', 'POST_..--block_id--helpers--scripts--trace_code')">Output format</button>
    </div><div id="POST_..--block_id--helpers--scripts--trace_codedescr" class="POST_..--block_id--helpers--scripts--trace_code tabcontent">
            <p>
            Run a piece of code in the current context, keeping a trace</p>
            </div>
  <div id="POST_..--block_id--helpers--scripts--trace_codeinput" class="POST_..--block_id--helpers--scripts--trace_code tabcontent">
    <pre>
    { "script": $micheline.michelson_v1.expression,
      "storage": $micheline.michelson_v1.expression,
      "input": $micheline.michelson_v1.expression,
      "amount": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
      "contract": $contract_id }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }</pre>
    </div>
  <div id="POST_..--block_id--helpers--scripts--trace_codeoutput" class="POST_..--block_id--helpers--scripts--trace_code tabcontent">
    <pre>
    { "storage": $micheline.michelson_v1.expression,
      "operations": [ $operation.alpha.internal_operation ... ],
      "trace":
        [ { "location": $micheline.location,
            "gas": $bignum || "unaccounted",
            "stack": [ $micheline.michelson_v1.expression ... ] } ... ],
      "big_map_diff"?:
        [ [ string, $micheline.michelson_v1.expression || null ] ... ] }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $micheline.location:
      /* Canonical location in a Micheline expression
         The location of a node in a Micheline expression tree in prefix order,
         with zero being the root and adding one for every basic node, sequence
         and primitive application. */
      integer ∈ [-2^30-2, 2^30+2]
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $operation.alpha.internal_operation:
      { "source": $contract_id,
        "nonce": integer ∈ [0, 2^16-1],
        "kind": "reveal",
        "public_key":
          string
          /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */ }
      || { "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "kind": "transaction",
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression }
      || { "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "kind": "origination",
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression } }
      || { "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "kind": "delegation",
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */ }</pre>
    </div>
  


.. _POST_..--block_id--helpers--scripts--typecheck_code :

**POST ../<block_id>/helpers/scripts/typecheck_code**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--typecheck_codedescr', 'POST_..--block_id--helpers--scripts--typecheck_code')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--typecheck_codeinput', 'POST_..--block_id--helpers--scripts--typecheck_code')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--typecheck_codeoutput', 'POST_..--block_id--helpers--scripts--typecheck_code')">Output format</button>
    </div><div id="POST_..--block_id--helpers--scripts--typecheck_codedescr" class="POST_..--block_id--helpers--scripts--typecheck_code tabcontent">
            <p>
            Typecheck a piece of code in the current context</p>
            </div>
  <div id="POST_..--block_id--helpers--scripts--typecheck_codeinput" class="POST_..--block_id--helpers--scripts--typecheck_code tabcontent">
    <pre>
    { "program": $micheline.michelson_v1.expression,
      "gas"?: $bignum }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }</pre>
    </div>
  <div id="POST_..--block_id--helpers--scripts--typecheck_codeoutput" class="POST_..--block_id--helpers--scripts--typecheck_code tabcontent">
    <pre>
    { "type_map":
        [ { "location": $micheline.location,
            "stack_before": [ $micheline.michelson_v1.expression ... ],
            "stack_after": [ $micheline.michelson_v1.expression ... ] } ... ],
      "gas": $bignum || "unaccounted" }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $micheline.location:
      /* Canonical location in a Micheline expression
         The location of a node in a Micheline expression tree in prefix order,
         with zero being the root and adding one for every basic node, sequence
         and primitive application. */
      integer ∈ [-2^30-2, 2^30+2]
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }</pre>
    </div>
  


.. _POST_..--block_id--helpers--scripts--typecheck_data :

**POST ../<block_id>/helpers/scripts/typecheck_data**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--typecheck_datadescr', 'POST_..--block_id--helpers--scripts--typecheck_data')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--typecheck_datainput', 'POST_..--block_id--helpers--scripts--typecheck_data')">Input format</button>
    <button class="tablinks" onclick="showTab(this, 'POST_..--block_id--helpers--scripts--typecheck_dataoutput', 'POST_..--block_id--helpers--scripts--typecheck_data')">Output format</button>
    </div><div id="POST_..--block_id--helpers--scripts--typecheck_datadescr" class="POST_..--block_id--helpers--scripts--typecheck_data tabcontent">
            <p>
            Check that some data expression is well formed and of a given type in the current context</p>
            </div>
  <div id="POST_..--block_id--helpers--scripts--typecheck_datainput" class="POST_..--block_id--helpers--scripts--typecheck_data tabcontent">
    <pre>
    { "data": $micheline.michelson_v1.expression,
      "type": $micheline.michelson_v1.expression,
      "gas"?: $bignum }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }</pre>
    </div>
  <div id="POST_..--block_id--helpers--scripts--typecheck_dataoutput" class="POST_..--block_id--helpers--scripts--typecheck_data tabcontent">
    <pre>
    { "gas": $bignum || "unaccounted" }
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string</pre>
    </div>
  


.. _GET_..--block_id--metadata :

**GET ../<block_id>/metadata**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--metadatadescr', 'GET_..--block_id--metadata')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--metadataoutput', 'GET_..--block_id--metadata')">Output format</button>
    </div><div id="GET_..--block_id--metadatadescr" class="GET_..--block_id--metadata tabcontent">
            <p>
            All the metadata associated to the block.</p>
            </div>
  <div id="GET_..--block_id--metadataoutput" class="GET_..--block_id--metadata tabcontent">
    <pre>
    $block_header_metadata
    $block_header_metadata:
      { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "next_protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "test_chain_status":
          /* Test chain status */
          { "status": "not_running" }
          || { "status": "forking",
               "protocol":
                 string
                 /* A Tezos protocol ID (Base58Check-encoded) */,
               "expiration":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
          || { "status": "running",
               "chain_id":
                 string
                 /* Network identifier (Base58Check-encoded) */,
               "genesis": string /* A block identifier (Base58Check-encoded) */,
               "protocol":
                 string
                 /* A Tezos protocol ID (Base58Check-encoded) */,
               "expiration":
                 /* timestamp */
                 $timestamp || integer ∈ [-2^31-2, 2^31+2] || string },
        "max_operations_ttl": integer ∈ [-2^30-2, 2^30+2],
        "max_operation_data_length": integer ∈ [-2^30-2, 2^30+2],
        "max_block_header_length": integer ∈ [-2^30-2, 2^30+2],
        "max_operation_list_length":
          [ { "max_size": integer ∈ [-2^30-2, 2^30+2],
              "max_op"?: integer ∈ [-2^30-2, 2^30+2] } ... ],
        "baker":
          string
          /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
        "level":
          { "level": integer ∈ [-2^31-2, 2^31+2],
            "level_position": integer ∈ [-2^31-2, 2^31+2],
            "cycle": integer ∈ [-2^31-2, 2^31+2],
            "cycle_position": integer ∈ [-2^31-2, 2^31+2],
            "voting_period": integer ∈ [-2^31-2, 2^31+2],
            "voting_period_position": integer ∈ [-2^31-2, 2^31+2],
            "expected_commitment": boolean },
        "voting_period_kind":
          "proposal" || "testing_vote" || "testing" || "promotion_vote" }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_..--block_id--metadata--max_block_header_length :

**GET ../<block_id>/metadata/max_block_header_length**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--metadata--max_block_header_lengthdescr', 'GET_..--block_id--metadata--max_block_header_length')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--metadata--max_block_header_lengthoutput', 'GET_..--block_id--metadata--max_block_header_length')">Output format</button>
    </div><div id="GET_..--block_id--metadata--max_block_header_lengthdescr" class="GET_..--block_id--metadata--max_block_header_length tabcontent">
            <p>
            ... FIXME ...</p>
            </div>
  <div id="GET_..--block_id--metadata--max_block_header_lengthoutput" class="GET_..--block_id--metadata--max_block_header_length tabcontent">
    <pre>
    integer ∈ [-2^30-2, 2^30+2]</pre>
    </div>
  


.. _GET_..--block_id--metadata--max_operation_data_length :

**GET ../<block_id>/metadata/max_operation_data_length**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--metadata--max_operation_data_lengthdescr', 'GET_..--block_id--metadata--max_operation_data_length')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--metadata--max_operation_data_lengthoutput', 'GET_..--block_id--metadata--max_operation_data_length')">Output format</button>
    </div><div id="GET_..--block_id--metadata--max_operation_data_lengthdescr" class="GET_..--block_id--metadata--max_operation_data_length tabcontent">
            <p>
            ... FIXME ...</p>
            </div>
  <div id="GET_..--block_id--metadata--max_operation_data_lengthoutput" class="GET_..--block_id--metadata--max_operation_data_length tabcontent">
    <pre>
    integer ∈ [-2^30-2, 2^30+2]</pre>
    </div>
  


.. _GET_..--block_id--metadata--max_operations_ttl :

**GET ../<block_id>/metadata/max_operations_ttl**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--metadata--max_operations_ttldescr', 'GET_..--block_id--metadata--max_operations_ttl')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--metadata--max_operations_ttloutput', 'GET_..--block_id--metadata--max_operations_ttl')">Output format</button>
    </div><div id="GET_..--block_id--metadata--max_operations_ttldescr" class="GET_..--block_id--metadata--max_operations_ttl tabcontent">
            <p>
            ... FIXME ...</p>
            </div>
  <div id="GET_..--block_id--metadata--max_operations_ttloutput" class="GET_..--block_id--metadata--max_operations_ttl tabcontent">
    <pre>
    integer ∈ [-2^30-2, 2^30+2]</pre>
    </div>
  


.. _GET_..--block_id--metadata--next_protocol_hash :

**GET ../<block_id>/metadata/next_protocol_hash**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--metadata--next_protocol_hashdescr', 'GET_..--block_id--metadata--next_protocol_hash')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--metadata--next_protocol_hashoutput', 'GET_..--block_id--metadata--next_protocol_hash')">Output format</button>
    </div><div id="GET_..--block_id--metadata--next_protocol_hashdescr" class="GET_..--block_id--metadata--next_protocol_hash tabcontent">
            <p>
            The protocol required to bake the next block.</p>
            </div>
  <div id="GET_..--block_id--metadata--next_protocol_hashoutput" class="GET_..--block_id--metadata--next_protocol_hash tabcontent">
    <pre>
    string
    /* A Tezos protocol ID (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_..--block_id--metadata--operation_list_quota :

**GET ../<block_id>/metadata/operation_list_quota**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--metadata--operation_list_quotadescr', 'GET_..--block_id--metadata--operation_list_quota')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--metadata--operation_list_quotaoutput', 'GET_..--block_id--metadata--operation_list_quota')">Output format</button>
    </div><div id="GET_..--block_id--metadata--operation_list_quotadescr" class="GET_..--block_id--metadata--operation_list_quota tabcontent">
            <p>
            ... FIXME ...</p>
            </div>
  <div id="GET_..--block_id--metadata--operation_list_quotaoutput" class="GET_..--block_id--metadata--operation_list_quota tabcontent">
    <pre>
    [ { "max_size": integer ∈ [-2^30-2, 2^30+2],
        "max_op"?: integer ∈ [-2^30-2, 2^30+2] } ... ]</pre>
    </div>
  


.. _GET_..--block_id--metadata--protocol_data :

**GET ../<block_id>/metadata/protocol_data**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--metadata--protocol_datadescr', 'GET_..--block_id--metadata--protocol_data')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--metadata--protocol_dataoutput', 'GET_..--block_id--metadata--protocol_data')">Output format</button>
    </div><div id="GET_..--block_id--metadata--protocol_datadescr" class="GET_..--block_id--metadata--protocol_data tabcontent">
            <p>
            The protocol-specific metadata associated to the block.</p>
            </div>
  <div id="GET_..--block_id--metadata--protocol_dataoutput" class="GET_..--block_id--metadata--protocol_data tabcontent">
    <pre>
    { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
      "baker":
        string
        /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
      "level":
        { "level": integer ∈ [-2^31-2, 2^31+2],
          "level_position": integer ∈ [-2^31-2, 2^31+2],
          "cycle": integer ∈ [-2^31-2, 2^31+2],
          "cycle_position": integer ∈ [-2^31-2, 2^31+2],
          "voting_period": integer ∈ [-2^31-2, 2^31+2],
          "voting_period_position": integer ∈ [-2^31-2, 2^31+2],
          "expected_commitment": boolean },
      "voting_period_kind":
        "proposal" || "testing_vote" || "testing" || "promotion_vote" }</pre>
    </div>
  


.. _GET_..--block_id--metadata--protocol_hash :

**GET ../<block_id>/metadata/protocol_hash**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--metadata--protocol_hashdescr', 'GET_..--block_id--metadata--protocol_hash')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--metadata--protocol_hashoutput', 'GET_..--block_id--metadata--protocol_hash')">Output format</button>
    </div><div id="GET_..--block_id--metadata--protocol_hashdescr" class="GET_..--block_id--metadata--protocol_hash tabcontent">
            <p>
            The protocol used to bake this block.</p>
            </div>
  <div id="GET_..--block_id--metadata--protocol_hashoutput" class="GET_..--block_id--metadata--protocol_hash tabcontent">
    <pre>
    string
    /* A Tezos protocol ID (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_..--block_id--metadata--test_chain_status :

**GET ../<block_id>/metadata/test_chain_status**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--metadata--test_chain_statusdescr', 'GET_..--block_id--metadata--test_chain_status')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--metadata--test_chain_statusoutput', 'GET_..--block_id--metadata--test_chain_status')">Output format</button>
    </div><div id="GET_..--block_id--metadata--test_chain_statusdescr" class="GET_..--block_id--metadata--test_chain_status tabcontent">
            <p>
            The status of the associated test chain.</p>
            </div>
  <div id="GET_..--block_id--metadata--test_chain_statusoutput" class="GET_..--block_id--metadata--test_chain_status tabcontent">
    <pre>
    /* Test chain status */
    { "status": "not_running" }
    || { "status": "forking",
         "protocol": string /* A Tezos protocol ID (Base58Check-encoded) */,
         "expiration":
           /* timestamp */
           $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
    || { "status": "running",
         "chain_id": string /* Network identifier (Base58Check-encoded) */,
         "genesis": string /* A block identifier (Base58Check-encoded) */,
         "protocol": string /* A Tezos protocol ID (Base58Check-encoded) */,
         "expiration":
           /* timestamp */
           $timestamp || integer ∈ [-2^31-2, 2^31+2] || string }
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  


.. _GET_..--block_id--operation_hashes :

**GET ../<block_id>/operation_hashes**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--operation_hashesdescr', 'GET_..--block_id--operation_hashes')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--operation_hashesoutput', 'GET_..--block_id--operation_hashes')">Output format</button>
    </div><div id="GET_..--block_id--operation_hashesdescr" class="GET_..--block_id--operation_hashes tabcontent">
            <p>
            The hashes of all the operations included in the block.</p>
            </div>
  <div id="GET_..--block_id--operation_hashesoutput" class="GET_..--block_id--operation_hashes tabcontent">
    <pre>
    [ [ string
    /* A Tezos operation ID (Base58Check-encoded) */ ... ] ... ]</pre>
    </div>
  

.. _GET_..--block_id--operation_hashes--list_offset :

**GET ../<block_id>/operation_hashes/<list_offset>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--operation_hashes--list_offsetdescr', 'GET_..--block_id--operation_hashes--list_offset')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--operation_hashes--list_offsetoutput', 'GET_..--block_id--operation_hashes--list_offset')">Output format</button>
    </div><div id="GET_..--block_id--operation_hashes--list_offsetdescr" class="GET_..--block_id--operation_hashes--list_offset tabcontent">
            <p>
            All the operations included in `n-th` validation pass of the block.</p>
            </div>
  <div id="GET_..--block_id--operation_hashes--list_offsetoutput" class="GET_..--block_id--operation_hashes--list_offset tabcontent">
    <pre>
    [ string
    /* A Tezos operation ID (Base58Check-encoded) */ ... ]</pre>
    </div>
  

.. _GET_..--block_id--operation_hashes--list_offset--operation_offset :

**GET ../<block_id>/operation_hashes/<list_offset>/<operation_offset>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--operation_hashes--list_offset--operation_offsetdescr', 'GET_..--block_id--operation_hashes--list_offset--operation_offset')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--operation_hashes--list_offset--operation_offsetoutput', 'GET_..--block_id--operation_hashes--list_offset--operation_offset')">Output format</button>
    </div><div id="GET_..--block_id--operation_hashes--list_offset--operation_offsetdescr" class="GET_..--block_id--operation_hashes--list_offset--operation_offset tabcontent">
            <p>
            The hash of then `m-th` operation in the `n-th` validation pass of the block.</p>
            </div>
  <div id="GET_..--block_id--operation_hashes--list_offset--operation_offsetoutput" class="GET_..--block_id--operation_hashes--list_offset--operation_offset tabcontent">
    <pre>
    string
    /* A Tezos operation ID (Base58Check-encoded) */</pre>
    </div>
  


.. _GET_..--block_id--operations :

**GET ../<block_id>/operations**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--operationsdescr', 'GET_..--block_id--operations')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--operationsoutput', 'GET_..--block_id--operations')">Output format</button>
    </div><div id="GET_..--block_id--operationsdescr" class="GET_..--block_id--operations tabcontent">
            <p>
            All the operations included in the block.</p>
            </div>
  <div id="GET_..--block_id--operationsoutput" class="GET_..--block_id--operations tabcontent">
    <pre>
    [ [ $operation ... ] ... ]
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_header.alpha.full_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $error:
      /* ... FIXME ... */
      any
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $operation:
      { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "chain_id": string /* Network identifier (Base58Check-encoded) */,
        "hash": string /* A Tezos operation ID (Base58Check-encoded) */,
        "branch": string /* A block identifier (Base58Check-encoded) */,
        "contents": [ $operation.alpha.operation_contents_and_result ... ],
        "signature"?:
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $operation.alpha.internal_operation_result:
      { "kind": "reveal",
        "source": $contract_id,
        "nonce": integer ∈ [0, 2^16-1],
        "public_key":
          string
          /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
        "result": $operation.alpha.operation_result.reveal }
      || { "kind": "transaction",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "result": $operation.alpha.operation_result.transaction }
      || { "kind": "origination",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "result": $operation.alpha.operation_result.origination }
      || { "kind": "delegation",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "result": $operation.alpha.operation_result.delegation }
    $operation.alpha.operation_contents_and_result:
      { "kind": "endorsement",
        "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ],
        "metadata":
          { "delegate":
              string
              /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
            "slots": [ integer ∈ [0, 255] ... ] } }
      || { "kind": "seed_nonce_revelation",
           "level": integer ∈ [-2^31-2, 2^31+2],
           "nonce": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_endorsement_evidence",
           "op1":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "op2":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_baking_evidence",
           "bh1": $block_header.alpha.full_header,
           "bh2": $block_header.alpha.full_header,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "activate_account",
           "pkh": string /* An Ed25519 public key hash (Base58Check-encoded) */,
           "secret": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "proposals",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposals":
             [ string
             /* A Tezos protocol ID (Base58Check-encoded) */ ... ],
           "metadata": {  } }
      || { "kind": "ballot",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposal": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "ballot": "nay" | "yay" | "pass",
           "metadata": {  } }
      || { "kind": "reveal",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "public_key":
             string
             /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.reveal,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "transaction",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.transaction,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "origination",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.origination,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "delegation",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.delegation,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "activate_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
      || { "kind": "activate_test_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
    $operation.alpha.operation_result.delegation:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.origination:
      { "status": "applied",
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.reveal:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.transaction:
      { "status": "applied",
        "storage"?: $micheline.michelson_v1.expression,
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation_metadata.alpha.balance_updates:
      [ { "kind": "contract",
          "contract": $contract_id,
          "credited":
            /* Amount in mutez */
            integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "contract",
             "contract": $contract_id,
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_..--block_id--operations--list_offset :

**GET ../<block_id>/operations/<list_offset>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--operations--list_offsetdescr', 'GET_..--block_id--operations--list_offset')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--operations--list_offsetoutput', 'GET_..--block_id--operations--list_offset')">Output format</button>
    </div><div id="GET_..--block_id--operations--list_offsetdescr" class="GET_..--block_id--operations--list_offset tabcontent">
            <p>
            All the operations included in `n-th` validation pass of the block.</p>
            </div>
  <div id="GET_..--block_id--operations--list_offsetoutput" class="GET_..--block_id--operations--list_offset tabcontent">
    <pre>
    [ $operation ... ]
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_header.alpha.full_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $error:
      /* ... FIXME ... */
      any
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $operation:
      { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "chain_id": string /* Network identifier (Base58Check-encoded) */,
        "hash": string /* A Tezos operation ID (Base58Check-encoded) */,
        "branch": string /* A block identifier (Base58Check-encoded) */,
        "contents": [ $operation.alpha.operation_contents_and_result ... ],
        "signature"?:
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $operation.alpha.internal_operation_result:
      { "kind": "reveal",
        "source": $contract_id,
        "nonce": integer ∈ [0, 2^16-1],
        "public_key":
          string
          /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
        "result": $operation.alpha.operation_result.reveal }
      || { "kind": "transaction",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "result": $operation.alpha.operation_result.transaction }
      || { "kind": "origination",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "result": $operation.alpha.operation_result.origination }
      || { "kind": "delegation",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "result": $operation.alpha.operation_result.delegation }
    $operation.alpha.operation_contents_and_result:
      { "kind": "endorsement",
        "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ],
        "metadata":
          { "delegate":
              string
              /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
            "slots": [ integer ∈ [0, 255] ... ] } }
      || { "kind": "seed_nonce_revelation",
           "level": integer ∈ [-2^31-2, 2^31+2],
           "nonce": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_endorsement_evidence",
           "op1":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "op2":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_baking_evidence",
           "bh1": $block_header.alpha.full_header,
           "bh2": $block_header.alpha.full_header,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "activate_account",
           "pkh": string /* An Ed25519 public key hash (Base58Check-encoded) */,
           "secret": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "proposals",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposals":
             [ string
             /* A Tezos protocol ID (Base58Check-encoded) */ ... ],
           "metadata": {  } }
      || { "kind": "ballot",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposal": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "ballot": "nay" | "yay" | "pass",
           "metadata": {  } }
      || { "kind": "reveal",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "public_key":
             string
             /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.reveal,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "transaction",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.transaction,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "origination",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.origination,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "delegation",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.delegation,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "activate_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
      || { "kind": "activate_test_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
    $operation.alpha.operation_result.delegation:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.origination:
      { "status": "applied",
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.reveal:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.transaction:
      { "status": "applied",
        "storage"?: $micheline.michelson_v1.expression,
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation_metadata.alpha.balance_updates:
      [ { "kind": "contract",
          "contract": $contract_id,
          "credited":
            /* Amount in mutez */
            integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "contract",
             "contract": $contract_id,
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  

.. _GET_..--block_id--operations--list_offset--operation_offset :

**GET ../<block_id>/operations/<list_offset>/<operation_offset>**

.. raw:: html
  
  <div class="tab"><button class="tablinks defaultOpen" onclick="showTab(this, 'GET_..--block_id--operations--list_offset--operation_offsetdescr', 'GET_..--block_id--operations--list_offset--operation_offset')">Description</button>
    <button class="tablinks" onclick="showTab(this, 'GET_..--block_id--operations--list_offset--operation_offsetoutput', 'GET_..--block_id--operations--list_offset--operation_offset')">Output format</button>
    </div><div id="GET_..--block_id--operations--list_offset--operation_offsetdescr" class="GET_..--block_id--operations--list_offset--operation_offset tabcontent">
            <p>
            The `m-th` operation in the `n-th` validation pass of the block.</p>
            </div>
  <div id="GET_..--block_id--operations--list_offset--operation_offsetoutput" class="GET_..--block_id--operations--list_offset--operation_offset tabcontent">
    <pre>
    $operation
    $bignum:
      /* Big number
         Decimal representation of a big number */
      string
    $block_header.alpha.full_header:
      { "level": integer ∈ [-2^31-2, 2^31+2],
        "proto": integer ∈ [0, 255],
        "predecessor": string /* A block identifier (Base58Check-encoded) */,
        "timestamp":
          /* timestamp */
          $timestamp || integer ∈ [-2^31-2, 2^31+2] || string,
        "validation_pass": integer ∈ [0, 255],
        "operations_hash":
          string
          /* A list of list of operations (Base58Check-encoded) */,
        "fitness": [ /^[a-zA-Z0-9]+$/ ... ] /* Tezos block fitness */,
        "context": string /* A hash of context (Base58Check-encoded) */,
        "priority": integer ∈ [0, 2^16-1],
        "proof_of_work_nonce": /^[a-zA-Z0-9]+$/,
        "seed_nonce_hash"?: string /* A nonce hash (Base58Check-encoded) */,
        "signature":
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $contract_id:
      /* A contract handle
         A contract notation as given to an RPC or inside scripts. Can be a
         base58 implicit contract hash or a base58 originated contract hash. */
      string
    $error:
      /* ... FIXME ... */
      any
    $micheline.michelson_v1.expression:
      /* Micheline expression (michelson_v1 variant) */
      { "int": $bignum }
      || { "string": string }
      || [ $micheline.michelson_v1.expression ... ]
      || { "prim":
             "CAR"
             | "LSR"
             | "ADDRESS"
             | "CREATE_CONTRACT"
             | "LSL"
             | "MANAGER"
             | "Elt"
             | "EDIV"
             | "OR"
             | "address"
             | "operation"
             | "ADD"
             | "AMOUNT"
             | "ISNAT"
             | "list"
             | "NEG"
             | "LOOP_LEFT"
             | "EMPTY_MAP"
             | "H"
             | "unit"
             | "SELF"
             | "Right"
             | "set"
             | "LAMBDA"
             | "MEM"
             | "ITER"
             | "storage"
             | "CONTRACT"
             | "CONS"
             | "XOR"
             | "STEPS_TO_QUOTA"
             | "False"
             | "AND"
             | "CREATE_ACCOUNT"
             | "COMPARE"
             | "or"
             | "GE"
             | "UPDATE"
             | "NEQ"
             | "SOURCE"
             | "int"
             | "timestamp"
             | "IMPLICIT_ACCOUNT"
             | "string"
             | "Unit"
             | "Some"
             | "ABS"
             | "PAIR"
             | "LT"
             | "CDR"
             | "EMPTY_SET"
             | "BALANCE"
             | "map"
             | "IF"
             | "SOME"
             | "MUL"
             | "NOT"
             | "NONE"
             | "None"
             | "DIP"
             | "GT"
             | "NOW"
             | "key_hash"
             | "NIL"
             | "contract"
             | "EXEC"
             | "SET_DELEGATE"
             | "UNIT"
             | "LOOP"
             | "SUB"
             | "CONCAT"
             | "DROP"
             | "MAP"
             | "RIGHT"
             | "LEFT"
             | "TRANSFER_TOKENS"
             | "INT"
             | "Left"
             | "big_map"
             | "signature"
             | "SWAP"
             | "mutez"
             | "EQ"
             | "GET"
             | "PUSH"
             | "option"
             | "IF_CONS"
             | "pair"
             | "nat"
             | "True"
             | "IF_LEFT"
             | "parameter"
             | "LE"
             | "HASH_KEY"
             | "SIZE"
             | "bool"
             | "Pair"
             | "lambda"
             | "FAIL"
             | "DUP"
             | "IF_NONE"
             | "key"
             | "code"
             | "CHECK_SIGNATURE",
           "args": [ $micheline.michelson_v1.expression ... ],
           "annot"?: string }
    $operation:
      { "protocol": "PsYLVpVvgbLhAhoqAkMFUo6gudkJ9weNXhUYCiLDzcUpFpkk8Wt",
        "chain_id": string /* Network identifier (Base58Check-encoded) */,
        "hash": string /* A Tezos operation ID (Base58Check-encoded) */,
        "branch": string /* A block identifier (Base58Check-encoded) */,
        "contents": [ $operation.alpha.operation_contents_and_result ... ],
        "signature"?:
          string
          /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ }
    $operation.alpha.internal_operation_result:
      { "kind": "reveal",
        "source": $contract_id,
        "nonce": integer ∈ [0, 2^16-1],
        "public_key":
          string
          /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
        "result": $operation.alpha.operation_result.reveal }
      || { "kind": "transaction",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "result": $operation.alpha.operation_result.transaction }
      || { "kind": "origination",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "result": $operation.alpha.operation_result.origination }
      || { "kind": "delegation",
           "source": $contract_id,
           "nonce": integer ∈ [0, 2^16-1],
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "result": $operation.alpha.operation_result.delegation }
    $operation.alpha.operation_contents_and_result:
      { "kind": "endorsement",
        "block": string /* A block identifier (Base58Check-encoded) */,
        "level": integer ∈ [-2^31-2, 2^31+2],
        "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ],
        "metadata":
          { "delegate":
              string
              /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
            "slots": [ integer ∈ [0, 255] ... ] } }
      || { "kind": "seed_nonce_revelation",
           "level": integer ∈ [-2^31-2, 2^31+2],
           "nonce": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_endorsement_evidence",
           "op1":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "op2":
             { "branch": string /* A block identifier (Base58Check-encoded) */,
               "operations":
                 { "kind": "endorsement",
                   "block":
                     string
                     /* A block identifier (Base58Check-encoded) */,
                   "level": integer ∈ [-2^31-2, 2^31+2],
                   "slots": [ integer ∈ [-2^30-2, 2^30+2] ... ] },
               "signature"?:
                 string
                 /* A Secp256k1 or Ed25519 signature (Base58Check-encoded) */ },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "double_baking_evidence",
           "bh1": $block_header.alpha.full_header,
           "bh2": $block_header.alpha.full_header,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "activate_account",
           "pkh": string /* An Ed25519 public key hash (Base58Check-encoded) */,
           "secret": /^[a-zA-Z0-9]+$/,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates } }
      || { "kind": "proposals",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposals":
             [ string
             /* A Tezos protocol ID (Base58Check-encoded) */ ... ],
           "metadata": {  } }
      || { "kind": "ballot",
           "source":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "period": integer ∈ [-2^31-2, 2^31+2],
           "proposal": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "ballot": "nay" | "yay" | "pass",
           "metadata": {  } }
      || { "kind": "reveal",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "public_key":
             string
             /* A Secp256k1 or Ed25519 public key (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.reveal,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "transaction",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "amount":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "destination": $contract_id,
           "parameters"?: $micheline.michelson_v1.expression,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.transaction,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "origination",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "manager_pubkey":
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "balance":
             /* Amount in mutez */
             integer ∈ [-2^31-2, 2^31+2] || string,
           "spendable"?: boolean,
           "delegatable"?: boolean,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "script"?:
             { "code": $micheline.michelson_v1.expression,
               "storage": $micheline.michelson_v1.expression },
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result":
                 $operation.alpha.operation_result.origination,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "delegation",
           "source": $contract_id,
           "fee": /* Amount in mutez */integer ∈ [-2^31-2, 2^31+2] || string,
           "counter": integer ∈ [-2^31-2, 2^31+2],
           "gas_limit": $bignum,
           "storage_limit": integer ∈ [-2^31-2, 2^31+2] || string,
           "delegate"?:
             string
             /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
           "metadata":
             { "balance_updates": $operation_metadata.alpha.balance_updates,
               "operation_result": $operation.alpha.operation_result.delegation,
               "internal_operation_results"?:
                 [ $operation.alpha.internal_operation_result ... ] } }
      || { "kind": "activate_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
      || { "kind": "activate_test_protocol",
           "hash": string /* A Tezos protocol ID (Base58Check-encoded) */,
           "metadata": {  } }
    $operation.alpha.operation_result.delegation:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.origination:
      { "status": "applied",
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.reveal:
      { "status": "applied" }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation.alpha.operation_result.transaction:
      { "status": "applied",
        "storage"?: $micheline.michelson_v1.expression,
        "balance_updates"?: $operation_metadata.alpha.balance_updates,
        "originated_contracts"?: [ $contract_id ... ],
        "consumed_gas"?: $bignum,
        "storage_size_diff"?: integer ∈ [-2^31-2, 2^31+2] || string }
      || { "status": "failed",
           "errors": [ $error ... ] }
      || { "status": "skipped" }
    $operation_metadata.alpha.balance_updates:
      [ { "kind": "contract",
          "contract": $contract_id,
          "credited":
            /* Amount in mutez */
            integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "contract",
             "contract": $contract_id,
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "rewards",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "fees",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "credited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string }
        || { "kind": "freezer",
             "category": "deposits",
             "delegate":
               string
               /* A Secp256k1 or Ed25519 public key hash (Base58Check-encoded) */,
             "level": integer ∈ [-2^31-2, 2^31+2],
             "debited":
               /* Amount in mutez */
               integer ∈ [-2^31-2, 2^31+2] || string } ... ]
    $timestamp:
      /* RFC 3339 formatted timestamp
         A date in human readble form as specified in RFC 3339. */
      string</pre>
    </div>
  



