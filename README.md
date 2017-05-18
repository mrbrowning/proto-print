proto-print
===========

Requires [cabal](https://www.haskell.org/cabal/) to build.

Sometimes you have short debug strings for protobufs (e.g. the kind returned from `TextFormat.shortDebugString()` in Java) in your logs that you'd like to see formatted in a reasonable way so you can understand the hierarchy. Maybe changing the software you're working on to print long-format debug strings involves an onerous rebuild, so you'd prefer to just take the short version and have it formatted readably. If this incredibly specific need applies to you, you're in luck! Just:
```shell
    $ echo 'message { field: VALUE submessage { subfield: "subvalue" num_field: 12.34 } }' > proto.txt
    $ cabal run proto.txt
    message {
        field: VALUE
        submessage {
            subfield: "subvalue"
            num_field: 12.34
        }
    }
```
