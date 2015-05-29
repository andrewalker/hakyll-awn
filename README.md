# andrewalker.net

[![Build Status](https://travis-ci.org/andrewalker/hakyll-awn.svg)](https://travis-ci.org/andrewalker/hakyll-awn)

Este é o código-fonte do meu [site pessoal](https://andrewalker.net).

## Instalando

Certifique-se que tem instalado ``cabal``. Caso não saiba como começar, instale
a [Haskell Platform](https://www.haskell.org/platform/).

``` shell

$ cabal sandbox init
$ cabal configure
$ cabal build
$ ./reload.sh

```

Em outro shell:

``` shell

$ cabal run server

```

Acesse [http://localhost:8000](http://localhost:8000) e navegue pelo site!
