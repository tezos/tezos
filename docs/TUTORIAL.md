# Tezos Code Tutorial

## Introduction

The purpose of this document is to help contributors get started with the Tezos
codebase. The code is organized in several layers in a way which largely reflects the philosophy of the project. It creates a very strict separation between the "node", which implements the network protocol described in the white paper and between the protocols themselves. Of course the seed protocol itself is a very important part of the Tezos project and it follows a similar organization. The economic protocol sits on top of a few layers of abstractions dealing primarily with storing and retrieving data from the current context.

## Overview of the source

This section presents a brief overview of the layout of the source files and their significance.

### node
The network shell
#### node/db
Persistent data structures used by the shell to store its state.
#### note/net
Connectivity for the gossip network and primitives to create RPC services 
#### node/shell
The shell itself
#### node/updater
Manage on-the-fly updates to the protocol

### proto
This is where the protocols live 
#### proto/environment

#### proto/current

### utils

### compiler

### client
### client/embedded

### Node, the network shell

### Storing the context

## Irmin

Tezos needs to store potentially different version of its context, corresponding to potentially different branches of the blockchain. This also implies the ability to roll back the changes made by any single block, and to make atomic changes to the structure on disk for eac block to avoid corruption. 

To that extent, Tezos borrows from the MirageOS project a module called [Irmin](https://github.com/mirage/irmin "Irmin")

> Irmin is a library to persist and synchronize distributed data structures both on-disk and in-memory. It enables a style of programming very similar to the Git workflow, where distributed nodes fork, fetch, merge and push data between each other. The general idea is that you want every active node to get a local (partial) copy of a global database and always be very explicit about how and when data is shared and migrated.

Caveat: although Tezos **is** a distributed ledger, it does **not** rely on Irmin's distributed store capabilities. For our purposes we only use Irmin as a local storage. The git structure is particularly well suited to represent the versionning implicit in the state of a blockchain based ledger. In fact, the context of Tezos can be observed by running "git checkout" in the data directory.

## Netbits and Jsont

Netbits and Jsont are modules which allow for the typesafe serialization of OCaml objects in a binary format and in Json (respectively). Importantly, it does not make of the potentially brittle representation created by the compiler and access through the Obj.magic function. Serializers are created using a set of type constructors defined in a GADT.


## (MISC STUFF TO BE ORGANIZED IN THE DOCUMENT)

The "Main" module represents the fixed interface between the economic protocol and the shell.

A protocol consists of several .ml and .mli files and a file name TEZOS_PROTOCOL which lists the modules in order of inclusion for the compilation. Lower level modules sit below high level modules.


What are \\_repr modules?

These modules handle the low level representation of a type, in particular its serialization in Json and in binary format. A module based on this type will often exist on top of it and provide functionality around that type. This provides finely grained layers of encapsulation to mininize the visible interfaces.

