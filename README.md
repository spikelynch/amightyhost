A Mighty Host!
==============

Haskell code which powers the Twitter bot [@amightyhost](https://twitter.com/amightyhost).

See http://bots.mikelynch.org/amightyhost/ for context.

The reusable part of this repository is the TextGen library, which is
now factored out to https://github.com/spikelynch/textgen

To install amightyhost, check it out with git, cd to the repo and:

    > stack build
    > stack install
    > ~/.local/bin/amightyhost-exe ./data


## Release notes

### 1.2

Uses textgen's streamlined vocab handler

### 1.1

First complete release
