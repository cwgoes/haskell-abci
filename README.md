# Haskell ABCI Server Library

Haskell library for writing [Application BlockChain Interface](https://github.com/tendermint/abci) (ABCI) servers. Licensed under BSD 3-clause (see LICENSE).

# Usage

Available as a [library on Hackage](https://hackage.haskell.org/package/haskell-abci).

If you're using [Stack](https://haskellstack.org), simply add *haskell-abci* to the build-depends in your project's Cabal file.

If you're using Cabal directly, *cabal install haskell-abci* should do the trick.

This library exposes a request-response API. Usage is simple:

1. Import the library

```haskell
import qualified Network.ABCI as ABCI
```

2. Define your application (for more information, see the [ABCI application development documentation](https://tendermint.com/docs/guides/app-development))

   See [the example counter app](app/Main.hs) for a template.

```haskell
app :: ABCI.Request -> IO ABCI.Response
app = undefined
```

3. Specify a host and port on which to bind the ABCI server, or use the defaults

```haskell
host :: String
host = ABCI.defaultHost

port :: Int
port = ABCI.defaultPort
```

4. Launch the server

```haskell
run :: IO ()
run = ABCI.serve host port app
```

# Development / Testing

You'll need [Stack](https://haskellstack.org/) and a relatively recent version of [protobuf](https://github.com/google/protobuf).

```bash
git clone https://github.com/cwgoes/haskell-abci.git
cd haskell-abci
stack build --install-ghc
```

Note that *src/types.proto* is used to generate a Haskell interface file whenever you run *stack build*.

Optionally, to also install the example counter application (*haskell-abci-counter*) in *~/.local/bin*:

```bash
stack install
```

Once you've installed *haskell-abci-counter*, to run the counter example application with the standard ABCI Golang tests:
(note that you'll need *~/.local/bin* on your shell path and [Tendermint](https://tendermint.com/docs/guides/install-from-source) installed)

```bash
git clone https://github.com/tendermint/abci.git
cd abci/tests/test_app
ABCI_APP="haskell-abci-counter" go run ./*.go
```
