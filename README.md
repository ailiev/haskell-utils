# Alexander Iliev small Haskell utility code

## Installation

### Requirements

The [Haskell Platform](http://hackage.haskell.org/platform/) has everything you
need and more, but here is a finer breakdown:

<table border="1">
<tr><td>Package</td><td>Debian/Ubuntu package name</td></tr>
<tr>  <td>Haskell compiler and core libraries</td>   <td>ghc6</td>  </tr>
<tr>  <td>Cabal</td>   <td>ghc6</td>  </tr>
<tr>  <td>Monad Transformer Library (MTL)</td>   <td>libghc6-mtl-dev</td>  </tr>
<tr>  <td>Parsec</td>   <td>libghc6-parsec2-dev</td>  </tr>
</table>

### Instructions

Use cabal. Either `cabal install` or using just the `Cabal` library:

    $ runhaskell Setup configure --user
    $ runhaskell Setup build
    $ runhaskell Setup install
