## Linkable Ring Signatures

[Linkable Ring Signatures](http://link.springer.com/chapter/10.1007%2F11424826_65#page-1) are cryptographic primitives that allow someone to **sign a message in name of a group**. Given a signature, it is possible to prove that a message was signed by a member of the group; yet, nobody can prove what specific member signed it. Moreover, if the same person signs two different messages, it is possible to prove those messages came from the same author.

It has several applications such as whistleblowing (leaking a secret in name of a group without being culpable), e-voting and e-cash (similar schemes are the underlying foundation of privacity-focused crypto-currencies such as Monero and Dash).

#### Usage

- [PureScript](https://github.com/MaiaVictor/purescript-lrs/blob/master/EXAMPLE_PURS.md)

- [JavaScript](https://github.com/MaiaVictor/purescript-lrs/blob/master/EXAMPLE_JS.md)

#### Disclaimer

This probably isn't fully reliable. It requires some expert's reviews and tests. I'd like to thank fgrieu and poncho, on Stack Overflow; campari, on Freenode; and many others who helped me enormously on understanding the paper.
