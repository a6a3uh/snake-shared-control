# Shared Control using POMDP

This is a Snake game example with shared control strategy. Shared control used when both human and machine should control the same process. This is aplicable in semi autonomus self driving car, assistive devices etc. 

# The purpose

The purpose of project is to evaluate my [POMDP](https://github.com/a6a3uh/POMDP) library.
The other reason is to learn Haskell. As this (along with POMDP library) is the first simple project written by me in Haskell. Many things are used here for learning purposes: Monads, Monad Transformers, Lenses, QuickCheck unittests, Haskell Stack etc. 

# The rules

Here the snake is controlled by an algorithm. There are several targets on boards each with particular bonus or penalty assigned. User sees those values while algorithm don't. User can issue direction commands to achieve his goal. The aim of algorithm is to guess right target with less imput possible.

# Build and run

This is done by [stack](https://docs.haskellstack.org/en/stable/README/) tool.

    stack build --install-ghc

Then run with 

    stack exec snake-shared-control-exe

# Configuration

Use `config.yaml` to change parameters. There are comments for each line in the file.

There are command line parameters controlling execution as well. To read more use:

    stack exec -- snake-shared-control-exe --help

# Thanks

Graphics part is havily inspired by [this](https://github.com/tfausak/haskell-snake-game) project.
