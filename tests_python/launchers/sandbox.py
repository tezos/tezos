from typing import Dict, List, Tuple
import tempfile
import shutil
import json
import os
import time
from daemons.node import Node
from daemons.baker import Baker
from daemons.endorser import Endorser
from client.client import Client


NODE = 'tezos-node'
CLIENT = 'tezos-client'
CLIENT_ADMIN = 'tezos-admin-client'
BAKER = 'tezos-baker'
ENDORSER = 'tezos-endorser'
TMP_DIR_PREFIX = 'tezos-sandbox.'


class Sandbox:
    """A sandbox acts as a node/daemons factory with nodes running in
    sandbox mode.

    Nodes and deamons are identified by an integer 0 <= node_id < num_peers,
    which is mapped to a port rpc + node_id, p2p + node_id.

    Nodes and deamons can be dynamically added or removed. Deamons are
    protocol specific. There can be more than one deamons for a given node,
    as long as they correspond to different protocol.

    Whenever a node has been added with `add_node()`, we can access to a
    corresponding client object `client()` to interact with this node.
    """

    def _wrap_path(self, binary: str, branch: str, proto="") -> str:
        """ construct binary name from branch and proto

        - follows tezos naming convention for binaries based on protocol
        - use branch as a prefix dir.
        """
        if proto:
            binary = f'{binary}-{proto}'
        res = os.path.join(self.binaries_path, branch, binary)  # type: str
        assert os.path.isfile(res), f'{res} is not a file'
        return res

    def __init__(self,
                 binaries_path: str,
                 identities: Dict[str, Dict[str, str]],
                 genesis_pk: str,
                 rpc: int = 18730,
                 p2p: int = 19730,
                 num_peers: int = 45,
                 log_dir: str = None):
        """
        Args:
            binaries_path (str): path to the binaries (client, node, baker,
                endorser). Typically, this parameter is TEZOS_HOME.
            identities (dict): identities known to all clients.
            genesis_pk (str): genesis activation public key
            rpc (int): base RPC port
            p2p (int): base P2P port
            num_peers (int): max number of peers
            log_dir (str): optional log directory for node/deamons logs

        Binaries contained in `binaries_path` are supposed to follow the
        naming conventions used in the Tezos codebase. For instance,
        `tezos-endorser-002-PsYLVpVv` is the endorser for protocol
        `-002-PsYLVpVv`.

        `identities` map aliases to dict of the form
        { 'identity': "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
            'public': "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
            'secret': "unencrypted:edsk3gUfUPy...") }
        """
        assert 1 <= num_peers <= 100
        assert os.path.isdir(binaries_path), f'{binaries_path} is not a dir'
        self.binaries_path = binaries_path
        self.log_dir = log_dir
        self.identities = dict(identities)
        self.rpc = rpc
        self.p2p = p2p
        self.num_peers = num_peers
        self.clients = {}  # type: Dict[int, Client]
        self.nodes = {}  # type: Dict[int, Node]
        # bakers for each protocol
        self.bakers = {}  # type: Dict[str, Dict[int, Baker]]
        self.endorsers = {}  # type: Dict[str, Dict[int, Endorser]]
        self.counter = 0
        self.logs = []  # type: List[str]
        self.sandbox_dir = None
        self.sandbox_file = None
        self._genesis_pk = genesis_pk

    def __enter__(self):
        """Create and initialize node dir"""
        sandbox_dir = tempfile.mkdtemp(prefix=TMP_DIR_PREFIX)
        self.sandbox_dir = sandbox_dir
        self.sandbox_file = f'{sandbox_dir}/sandbox_file.json'
        with open(self.sandbox_file, 'w+') as file:
            file.write(json.dumps({'genesis_pubkey': self._genesis_pk}))
        return self

    def add_node(self,
                 node_id: int,
                 peers: List[int] = None,
                 params: List[str] = None,
                 log_levels: Dict[str, str] = None,
                 private: bool = True,
                 config_client: bool = True,
                 use_tls: Tuple[str, str] = None,
                 branch: str = "") -> None:
        """ Launches new node with given node_id and initializes client

        Args:
            node_id (int): id of the node, defines its RPC/P2P port and serves
                           as an identifier for client and deamons
            peer (list): id of peers initialized trusted by this nodes
            params (list): list of additional parameters to run the node
            log_levels (dict): log levels. e.g. {"p2p.connection-pool":"debug"}
            private (bool): enable private mode, default=True
            config_client (bool): initialize client with sandbox identities,
                                  default=True
            branch (str): sub-dir where to lookup the node and client
                          binary, default = "". Allows execution of different
                          versions of nodes.

        This launches a node with default sandbox parameters

        tezos-node run
           --data-dir TMP_DIR
           --no-bootstrap-peers
           --sandbox=SANDBOX_FILE
           --peer TRUSTED_PEER_1 ... --peer TRUSTED_PEER_n #
           --private-mode # if private is True
           --connections 3 # overriden by params if not None
        """
        assert node_id not in self.nodes, f'Already a node for id={node_id}'
        rpc_node = self.rpc + node_id
        p2p_node = self.p2p + node_id
        assert 0 <= node_id < self.num_peers, f'{node_id} outside bounds'
        if peers is None:
            peers = list(range(self.num_peers))
        else:
            assert all(0 <= peer < self.num_peers for peer in peers)

        log_file = None
        if self.log_dir:
            log_file = f'{self.log_dir}/node{node_id}_{self.counter}.txt'
            self.logs.append(log_file)
            self.counter += 1

        if params is None:
            # TODO this option is confusing as it sets several parameters at
            # once
            params = ['--connections', '3']
        if private:
            params = params + ['--private-mode']
        peers_rpc = [self.p2p + p for p in peers]
        node_bin = self._wrap_path(NODE, branch)
        local_admin_client = self._wrap_path(CLIENT_ADMIN, branch)
        local_client = self._wrap_path(CLIENT, branch)
        node = Node(node_bin, self.sandbox_file,
                    p2p_port=p2p_node, rpc_port=rpc_node,
                    peers=peers_rpc, log_file=log_file, params=params,
                    log_levels=log_levels, use_tls=use_tls)
        client = Client(local_client, local_admin_client, rpc_port=rpc_node,
                        use_tls=bool(use_tls))
        time.sleep(0.1)
        # make sure node didn't fail at startup
        assert node.poll() is None, 'Seems node failed at startup'

        # don't wait for confirmation
        client.run(['-w', 'none', 'config', 'update'])
        if config_client:
            for name, iden in self.identities.items():
                client.import_secret_key(name, iden['secret'])
        self.nodes[node_id] = node
        self.clients[node_id] = client

    def add_baker(self,
                  node_id: int,
                  account: str,
                  proto: str,
                  params: List[str] = None,
                  branch: str = "") -> None:
        """
        Add a baker associated to a node.

        Args:
            node_id (int): id of corresponding node
            account (str): account to bake for
            proto (str): name of protocol, used to determine the binary to
                         use. E.g. 'alpha` for `tezos-baker-alpha`.
            params (list): additioonal parameters
            branch (str): see branch parameter for `add_node()`
        """
        assert node_id in self.nodes, f'No node running with id={node_id}'
        if proto not in self.bakers:
            self.bakers[proto] = {}
        assert_msg = f'Already a baker for proto={proto} and id={node_id}'
        assert node_id not in self.bakers[proto], assert_msg
        baker_path = self._wrap_path(BAKER, branch, proto)
        node = self.nodes[node_id]
        client = self.clients[node_id]
        rpc_node = node.rpc_port

        log_file = None
        if self.log_dir:
            log_file = (f'{self.log_dir}/baker-{proto}_{node_id}_#'
                        f'{self.counter}.txt')
            self.logs.append(log_file)
            self.counter += 1

        baker = Baker(baker_path, rpc_node, client.base_dir, node.node_dir,
                      account, params=params, log_file=log_file)
        time.sleep(0.1)
        assert baker.poll() is None, 'seems baker failed at startup'
        self.bakers[proto][node_id] = baker

    def add_endorser(self,
                     node_id: int,
                     account: str,
                     proto: str,
                     endorsement_delay: float = 0.,
                     branch: str = "") -> None:
        """
        Add an endorser associated to a node.

        Args:
            node_id (int): id of corresponding node
            account (str): account to endorse for
            proto (str): name of protocol, used to determine the binary to
                         use. E.g. 'alpha` for `tezos-endorser-alpha`.
            params (list): additiosonal parameters
            branch (str): see branch parameter for `add_node()`
        """
        assert node_id in self.nodes, f'No node running with id={node_id}'
        if proto not in self.endorsers:
            self.endorsers[proto] = {}
        account_param = []  # type: List[str]
        if account is not None:
            account_param = [account]

        assert_msg = f'Already an endorser for proto={proto} and id={node_id}'
        assert node_id not in self.endorsers[proto], assert_msg
        endorser_path = self._wrap_path(ENDORSER, branch, proto)
        node = self.nodes[node_id]
        client = self.clients[node_id]
        rpc_node = node.rpc_port

        log_file = None
        if self.log_dir:
            log_file = (f'{self.log_dir}/endorser-{proto}_{node_id}_#'
                        f'{self.counter}.txt')
            self.logs.append(log_file)
            self.counter += 1
        params = (['run'] + account_param +
                  ['--endorsement-delay', str(endorsement_delay)])
        endorser = Endorser(endorser_path, rpc_node, client.base_dir,
                            params=params, log_file=log_file)
        time.sleep(0.1)
        assert endorser.poll() is None, 'seems endorser failed at startup'
        self.endorsers[proto][node_id] = endorser

    def rm_baker(self, node_id: int, proto: str) -> None:
        """Kill baker for given node_id and proto"""
        baker = self.bakers[proto][node_id]
        del self.bakers[proto][node_id]
        baker.kill()

    def rm_endorser(self, node_id: int, proto: str) -> None:
        """Kill endorser for given node_id and proto"""
        endorser = self.bakers[proto][node_id]
        del self.endorsers[proto][node_id]
        endorser.kill()

    def rm_node(self, node_id: int) -> None:
        """Kill node for given node_id"""
        node = self.nodes[node_id]
        del self.nodes[node_id]
        del self.clients[node_id]
        node.kill()
        node.cleanup()

    def client(self, node_id: int) -> Client:
        """ Returns client for node node_id """
        return self.clients[node_id]

    def node(self, node_id: int) -> Node:
        """ Returns node for node_id """
        return self.nodes[node_id]

    def baker(self, node_id: int, proto: str) -> Baker:
        """ Returns baker for node node_i and proto """
        return self.bakers[proto][node_id]

    def all_clients(self) -> List[Client]:
        """ Returns the list of all clients to an active node
           (no particular order)."""
        return list(self.clients.values())

    def all_nodes(self) -> List[Node]:
        """ Returns the list of all active nodes (no particular order)."""
        return list(self.nodes.values())

    def __exit__(self, *exc):
        self.cleanup()

    def cleanup(self):
        """Kill all deamons and cleanup temp dirs."""
        for node in self.nodes.values():
            node.kill()
            node.cleanup()
        for proto in self.bakers:
            for baker in self.bakers[proto].values():
                baker.kill()
        for proto in self.endorsers:
            for endorser in self.endorsers[proto].values():
                endorser.kill()
        for client in self.clients.values():
            client.cleanup()
        shutil.rmtree(self.sandbox_dir)

    def are_daemons_alive(self) -> bool:
        """ Returns True iff all started deamons/nodes are still alive """
        daemons_alive = True
        for node_id, node in self.nodes.items():
            if node.poll() is not None:
                print(f'# node {node_id} has failed')
                daemons_alive = False
        for proto in self.bakers:
            for baker_id, baker in self.bakers[proto].items():
                if baker.poll() is not None:
                    print(f'# baker {baker_id} for proto {proto} has failed')
                    daemons_alive = False
        for proto in self.endorsers:
            for endo_id, endorser in self.endorsers[proto].items():
                if endorser.poll() is not None:
                    print(f'# endorser {endo_id} for proto {proto} has failed')
                    daemons_alive = False
        return daemons_alive


class SandboxMultiBranch(Sandbox):
    """Specialized version of `Sandbox` using binaries with different versions.

    Binaries are looked up according to a map from node_id to branch

    For instance,  if we define `branch_map` as:

    branch_map = {i: 'zeronet' if i % 2 == 0 else 'alphanet'
                  for i in range(20)}

    Nodes/client for even ids will be looked up in `binaries_path/zeronet`
    Nodes/client for odd ids will be looked up in `binaries_path/alphanet`

    One advantage of using `SandboxMultibranch` rather than `Sandbox` is that
    we can sometimes run the same tests with different binaries revision by
    simply changing the sandbox.
    """

    def __init__(self,
                 binaries_path: str,
                 identities: Dict[str, Dict[str, str]],
                 genesis_pk: str,
                 branch_map: Dict[int, str],
                 rpc: int = 18730,
                 p2p: int = 19730,
                 num_peers: int = 45,
                 log_dir: str = None):
        """Same semantics as Sandbox class, plus a `branch_map` parameter"""
        super().__init__(binaries_path,
                         identities,
                         genesis_pk,
                         rpc,
                         p2p,
                         num_peers,
                         log_dir)
        self._branch_map = branch_map
        for branch in list(branch_map.values()):
            error_msg = f'{binaries_path}/{branch} not a dir'
            assert os.path.isdir(f'{binaries_path}/{branch}'), error_msg

    def add_baker(self,
                  node_id: int,
                  account: str,
                  proto: str,
                  params: List[str] = None,
                  branch: str = "") -> None:
        """branch is overridden by branch_map"""
        branch = self._branch_map[node_id]
        super().add_baker(node_id, account, proto, params, branch)

    def add_endorser(self,
                     node_id: int,
                     account: str,
                     proto: str,
                     endorsement_delay: float = 0.,
                     branch: str = "") -> None:
        """branchs is overridden by branch_map"""
        branch = self._branch_map[node_id]
        super().add_endorser(node_id, account, proto, endorsement_delay,
                             branch)

    def add_node(self,
                 node_id: int,
                 peers: List[int] = None,
                 params: List[str] = None,
                 log_levels: Dict[str, str] = None,
                 private: bool = True,
                 config_client: bool = True,
                 use_tls: Tuple[str, str] = None,
                 branch: str = "") -> None:
        assert not branch
        branch = self._branch_map[node_id]
        super().add_node(node_id, peers, params, log_levels, private,
                         config_client, use_tls, branch)
