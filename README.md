# [andrewalker.net](https://andrewalker.net)
[![Build Status](https://travis-ci.org/andrewalker/hakyll-awn.svg)](https://travis-ci.org/andrewalker/hakyll-awn)

Certifique-se que tem instalado ``cabal``. Caso não saiba como começar, instale
a [Haskell Platform](https://www.haskell.org/platform/).

``` shell

$ cabal sandbox init
$ cabal configure
$ cabal build
$ cabal run rebuild
$ cabal run server

```

O site fica acessível em [http://localhost:8000](http://localhost:8000).

Para fazer alterações, utilize `./bin/watch`, e deixe `cabal run server` rodando
em outra shell.
