```
    # Look at Config.hs for configurable items like the port

    $ cabal sandbox init
    $ cabal install --only-dependencies
    $ cabal configure
    $ cabal build
    $ dist/build/zalora/zalora

    # Running the web service
    $ runghc -- -package-db --ghc-arg=.cabal-sandbox/x86_64-linux-ghc-7.8.2-packages.conf.d/ Main.hs

    # POSTing new items
    $ curl -H "Content-Type: application/json" -d "$(<   red.json)" http://localhost:8080/
    $ curl -H "Content-Type: application/json" -d "$(< green.json)" http://localhost:8080/
    $ curl -H "Content-Type: application/json" -d "$(<  blue.json)" http://localhost:8080/

    # Getting an item
    $ curl http://localhost:8080/item/${key}

    # Getting a list of items
    $ curl http://localhost:8080/items
```
