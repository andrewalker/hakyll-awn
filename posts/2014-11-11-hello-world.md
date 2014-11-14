---
title: Hello World
tags: perl, haskell
---

Vou testar syntax highlighting com ``pandoc`` e ``markdown``.

<!--more-->

## Haskell code

~~~~~~ {.haskell}

main = putStrLn "Hello World!"

~~~~~~

## Perl code

~~~~~~ {.perl}

#!/usr/bin/env perl
use warnings;
use strict;

foo("Hello World\n");

sub foo {
    my $var = shift(@_);
    print $var;
}

~~~~~~
