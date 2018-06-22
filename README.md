# ocaml-xi-rope [![Build status](https://travis-ci.org/cfcs/ocaml-xi-rope.svg?branch=master)](https://travis-ci.org/cfcs/ocaml-xi-rope)

The aim of this library is to implement a (immutable)
["rope" data structure](https://en.wikipedia.org/wiki/Rope_(data_structure))
based on the one used in [xi-editor](https://github.com/google/xi-editor)
(authored by Raph Levien).

Xi's rope type is a
[CRDT](https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type)
(based on [WOOT](https://hal.inria.fr/inria-00108523/document)) permitting
decentralized collaborative editing _without_ vector clocks.

The ropes are currently implemented and exposed using
[Daniel Bünzli](http://erratique.ch/contact.en)'s
[`pvec` library](https://github.com/dbuenzli/pvec).

My hope is to provide a set of libraries on top of this data structure
supporting *n-way merges* of application configuration state for MirageOS
unikernels, and a collaborative text editor widget usable with
[`notty`](https://github.com/pqwy/notty)
and [`mirage-framebuffer`](https://github.com/cfcs/mirage-framebuffer/).

Please report any issues or requests for additional features on the
[issue tracker](https://github.com/cfcs/ocaml-xi-rope/issues/).

# Limitations

This repository is currently available mainly as a proof of concept,
and to enable me to receive feedback on the implementation.
As such, this is not *production-ready:*

- While the data structures are immutable, it is still quite possible to supply
  invalid data (cyclic graphs, colliding unique IDs, ..)

- instead of using `result` types to signal invalid operations,
  the implementation throws exceptions left and right.

- While the **construction**, **merging** and **snapshotting** are implemented,
  my underlying implementation is pretty inefficient and some work on that will
  be needed to support large operations with reasonable performance.

- **insertion** is supported, but my algorithms, and the API, to handle
  insertions are fairly limited and cumbersome to work with.

- The test suite is missing.

- Nice things like Levien's line-wrapping and paging algorithms are
  **not** implemented.

# Installing the dependencies

```
opam pin add -ny pvec https://github.com/dbuenzli/pvec.git
opam install alcotest fmt logs pvec qcheck rresult uchar
```

# Further reading
Below is a list of resources I found helpful and interesting:
- [Levien on OT, TP2, CRDTs](https://medium.com/@raphlinus/towards-a-unified-theory-of-operational-transformation-and-crdt-70485876f72f)
- [overview of Xi's CRDT](https://google.github.io/xi-editor/docs/crdt.html)
  - [more details](https://google.github.io/xi-editor/docs/crdt-details.html)
  - [Xi documentation](http://google.github.io/xi-editor/docs.html)
- [YouTube presentation on Xi](https://www.youtube.com/watch?v=SKtQgFBRUvQ)
- [Nathan Sobo's description of the merge operations](https://gist.github.com/nathansobo/a15266a30ed433052a915605596c5ff4)