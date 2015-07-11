# Connect Four

This is a Haskell implementation for the classic Connect Four game, also known as Four in a Line. The game was first sold under the famous Connect Four trademark by Milton Bradley in February 1974. Haskell is purely functional programming language. A great starting point for learning about Haskell might be the awesome "Learn You a Haskell for Great Good!" book which you may grab right [here](http://learnyouahaskell.com/) for free. For further information regarding the haskell programming language, please enter [here](https://www.haskell.org/)

# Installation

Make sure you have installed the Haskell platform. This includes both the Glasgow Haskell Compiler (GHC) and the Cabal package manager. You may download it all form [here](https://www.haskell.org/platform/)

```bash
$ git clone https://github.com/Tsur/connect4 && cd connect4
$ cabal sandbox init # You might want to set it up locally
$ cabal install connect4.cabal
$ .cabal_sandbox/bin/build/connect4/connect4
$ cabal sandbox delete # uninstall the application
```

If you have cabalg, just run:

```bash
$ cabalg https://github.com/Tsur/connect4
```

# Developers

You need to install some dependeces before going on as [Wx](https://wiki.haskell.org/WxHaskell)

Then just configure and build the project:

```bash
$ cabal sandbox init
$ cabal install wx cabal-macosx
$ cabal configure
$ cabal build
```

Now you can run connect4 game:

```bash
$ ./dist/build/connect4/connect4
```

For running it without building, just go with:

```bash
$ ghc -package wx -o connect4 Main.hs
$ ./connect4
```

