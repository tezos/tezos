Before submitting your issue, ask this question: is this a bug in the Tezos codebase or a personal problem for which you need support? Here are a few examples of each:

Personal problem:
* Lost or compromised key
* Fundraiser issues
* Questions about how to use Tezos
* Feature requests
* Questions about the state of development

These sorts of questions should be asked on the [riot chat](https://riot.im/app/#/room/#tezos:matrix.org). People there will be happy to assist you.

Bugs:
* Crashes or exceptions in the node
* Defaults that cause failures
* Missing documentation
* Build failures

Make sure to give your issue a descriptive title. We should get the general idea of the problem just from reading the title. Avoid words like "weird", "strange", and "unexpected". Instead, spell out the strange behavior in as much detail as possible. As an example, "Michelson: lists are reversed" is significantly better than "List handling is weird". Unless your investigations have revealed the source of the bug, do not speculate on its cause or severity. The easier it is for us to understand your bug the easier it is for us to fix.

### Environment (Alphanet, build from source, or both)

Please specify the version of the code you were running when the bug appeared.

If you've built the program from source, you can find the commit hash via the following command:

`git log -1 --format=format:%H`

If you're running the alphanet, the status output is extremely useful:

`./alphanet.sh status`

### Expected behavior
What you expected to happen. 

### Actual behavior

What actually happened.

### Steps to reproduce

Please provide the command that led to the issue. Copy and paste the command line and the output into the issue and attach any files we'll need to reproduce the bug. Screenshots are much harder to deal with because we cannot rerun your commands or see the entire setup.

Whenever possible, provide the smallest amount of code needed to produce the bug dependably. If you cannot reproduce the bug, we likely will not be able to either.

If you had a problem while trying to build Tezos from source, please include the output of `opam list -i` and any error messages that you saw while building. If you ran a second command which fixed the problem, provide us with the error you saw initially in addition to telling us how you fixed the bug.

### Logs
Please include logs with your bug report whenever possible.


On the alphanet, you can access the log from the node, baker, and endorser using the following commands:
* `./alphanet.sh node log`
* `./alphanet.sh baker log`
* `./alphanet.sh endorser log`

If you've encountered the bug when using the sandboxed node initialization scripts, there should be a file in the directory called `LOG.N`, where `N` is the number with which you started the node. Please attach that log to the bug report.
