# andrewalker.net

[![Build Status](https://travis-ci.org/andrewalker/hakyll-awn.svg)](https://travis-ci.org/andrewalker/hakyll-awn)

Este é o código-fonte do meu [site pessoal](https://andrewalker.net). Ainda não
possui muito conteúdo, estou concentrado ajustando os detalhes de programação,
design, e infraestrutura. Inclusive, os primeiros posts serão predominantemente
explicando como fiz isso.

## Instalando

Certifique-se que tem instalado ``cabal``. Caso não saiba como começar, instale
a [Haskell Platform](https://www.haskell.org/platform/).

``` shell

$ cabal sandbox init
$ cabal configure
$ cabal build
$ cabal run build
$ cabal run watch

```

Acesse [http://localhost:8000](http://localhost:8000) e navegue pelo site!
