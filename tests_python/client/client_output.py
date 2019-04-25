"""Structured representation of client output."""
from typing import List
import re
import json
# TODO This is incomplete. Add additional attributes and result classes as
#      they are needed


class InvalidClientOutput(Exception):
    """Raised when client output couldn't be parsed."""

    def __init__(self, client_output: str):
        super().__init__(self)
        self.client_output = client_output


class EndorseResult:
    """Result of a 'endorse for' operation."""

    def __init__(self, client_output: str):
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.operation_hash = match.groups()[0]


class TransferResult:
    """Result of a 'transfer' operation."""

    def __init__(self, client_output: str):
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.operation_hash = match.groups()[0]
        pattern = r"--branch ?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.branch_hash = match.groups()[0]


class GetReceiptResult:
    """Result of 'get receipt' operation.

    If operation wasn't found, 'black_hash' is set to None.
    """

    def __init__(self, client_output: str):
        if client_output == "Couldn't find operation\n":
            self.block_hash = None
            return
        pattern = r"Operation found in block: ?(\w*) "
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.block_hash = match.groups()[0]


class RunScriptResult:
    """Result of a 'get script' operation."""

    def __init__(self, client_output: str):
        pattern = r"^storage\n\s*(.*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.storage = match.groups()[0]


class OriginationResult:
    """Result of an 'originate contract' operation."""

    def __init__(self, client_output: str):
        pattern = r"New contract ?(\w*) originated"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.contract = match.groups()[0]
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput
        self.operation_hash = match.groups()[0]


class SubmitProposalsResult:
    """Result of an 'submit proposals' operation."""

    def __init__(self, client_output: str):
        pattern = r"Operation hash is '?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.operation_hash = match.groups()[0]


class BakeForResult:
    """Result of a 'baker for' operation."""

    def __init__(self, client_output: str):
        pattern = r"Injected block ?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.block_hash = match.groups()[0]


class ActivationResult:
    """Result of 'activate protocol' command"""

    def __init__(self, client_output: str):
        pattern = r"Injected ?(\w*)"
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.block_hash = match.groups()[0]


class WaitForResult:
    """Result of a 'wait for' command."""

    def __init__(self, client_output: str):
        pattern = r"Operation found in block: ?(\w*) "
        match = re.search(pattern, client_output)
        if match is None:
            raise InvalidClientOutput(client_output)
        self.block_hash = match.groups()[0]


def extract_rpc_answer(client_output: str) -> dict:
    """Convert json client output to a dict representation.

    For some RPC, the answer isn't json, in that case, the client_output
    can be retrieved from the InvalidClientOutput exception"""
    try:
        return json.loads(client_output)
    except json.JSONDecodeError:
        raise InvalidClientOutput(client_output)


def extract_balance(client_output: str) -> float:
    """Extract float balance from the output of 'get_balance' operation."""
    try:
        return float(client_output[:-3])
    except Exception:
        raise InvalidClientOutput(client_output)


def extract_protocols(client_output: str) -> List[str]:
    """Extract protocol from the output of 'get_protocols' operation."""
    return client_output.split()
